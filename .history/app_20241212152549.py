import json
import os
import subprocess
import networkx as nx
from typing import Dict, List
from groq import Groq
from pydantic import BaseModel

# Initialize Groq AI
groq = Groq(api_key="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")

# Define Pydantic models
class FileSummary(BaseModel):
    file_name: str
    summary: str

class DependencyGraph(BaseModel):
    nodes: List[str]
    edges: Dict[str, List[str]]

class ConvertedFile(BaseModel):
    file_name: str
    python_code: str


class Code(BaseModel):
    completePythonCode: str 
    singleTestAssertLine : str

# Summarize each file
def summarize_file(file_path: str) -> FileSummary:
    """Summarize the content of a COBOL file using Groq."""
    with open(file_path, 'r') as file:
        content = file.read()

    response = groq.chat.completions.create(
        messages=[
            {"role": "system", "content": "You are a COBOL summarizer. Summarize the following COBOL code and tell which file it depends on:"},
            {"role": "user", "content": content},
        ],
        model="mixtral-8x7b-32768",
        temperature=0,
    )

    summary = response.choices[0].message.content.strip()
    
    with open(f"./cobol/summary/{os.path.basename(file_path)}", 'w') as file:
        file.write(summary)
        
    return FileSummary(file_name=os.path.basename(file_path), summary=summary)


# Analyze dependencies and create a graph
def analyze_dependencies(summaries: List[FileSummary]) -> DependencyGraph:
    """Analyze dependencies between files based on summaries."""
    graph = nx.DiGraph()

    for summary in summaries:
        graph.add_node(summary.file_name)
        for other_summary in summaries:
            if other_summary.file_name != summary.file_name and other_summary.file_name in summary.summary:
                graph.add_edge(other_summary.file_name, summary.file_name)

    return DependencyGraph(
        nodes=list(graph.nodes),
        edges={node: list(graph.successors(node)) for node in graph.nodes},
    )

def imporve_recipe( code : str , error : str , testCase : str) -> Code:
    chat_completion = groq.chat.completions.create(
        messages=[
            {
                "role": "system",
                "content": "You are an SWE engineer specialized in converting legacy (Cobol) code to new code (Python). correct the following code :" + code + " This is the error caused by the code Error : " + error + " Make sure the code is correct againts the following testCase :" + testCase + " provide the following in JSON\n"
                f" The JSON object must use the schema: {json.dumps(Code.model_json_schema(), indent=2)}",
            },
            {
                "role": "user",
                "content": f"Give me a single assert TestCase the above python code . The testCase should be an assert Test in python for the aboce code",
            },
        ],
        model="mixtral-8x7b-32768",
        temperature=0,
        # Streaming is not supported in JSON mode
        stream=False,
        # Enable JSON mode by setting the response format
        response_format={"type": "json_object"},
    )
    return Code.model_validate_json(chat_completion.choices[0].message.content)

def run_code(code , testCase):
    try:
        # Run the code in the terminal
        # result = subprocess.run(["python", "-c", code], input=test_case_input.encode(), capture_output=True, text=True)
        test_code : str = code + "\n" + testCase.replace("```python\n" , "").replace("```python" , "")

        test_code = test_code.replace("```" , "").replace("```python\n" , "").replace("```python" , "")
        result = subprocess.run(["python", "-c", test_code],   capture_output=True, text=True)

        # Check if there's any error
        if result.returncode != 0 or result == "":
            error_message = result.stderr
            print("error_message : " , error_message)
            print("------------------------")
            # addToFile("error.txt" ,  error_message)

            return False, error_message
        else:
            output = result.stdout
            print("output-code is :" , output)
            print("------------------------")
            return True, output.strip()  # Strip any trailing newline from output

            # if output == test_case_output:
            #     return True, output.strip()  # Strip any trailing newline from output
            # else:
            #     raise Exception
    except Exception as e:
        return False, str(e)
    

def aiAgent(code_string  , testCase):

    success , result = run_code(code_string   , testCase)
    if success:
        print("result :------", result)
        saveFile("ai_agent_created_Code" , code_string)
        return result
    else:
        error = result
        resulted_Code = imporve_recipe(code_string , error , testCase=testCase )
        aiAgent(resulted_Code.completePythonCode  , testCase)
        
def saveFile(filename, code):
    with open(filename, 'w') as file:
        file.write(code)

def get_recipe( file_path: str, context: str) -> Code:
    
    with open(file_path, 'r') as file:
        content = file.read()
    chat_completion = groq.chat.completions.create(
        messages=[
            {
                "role": "system",
                "content": f" Context: {context}\n. You are an Software developer engineer specialized in converting legacy (Cobol) code to new code (Python) . create me an python-Code for the following given cobol code the code is as follows:" + content + "convert this code into  JSON\n"
                f" The JSON object must use the schema: {json.dumps(Code.model_json_schema(), indent=2)}",
            },
                        {"role": "user", "content": f"Give me a single line assert TestCase the above python code . The testCase should be an assert Test in python for the above code"},

            
        ],
        model="mixtral-8x7b-32768",
        temperature=0,
        # Streaming is not supported in JSON mode
        stream=False,
        # Enable JSON mode by setting the response format
        response_format={"type": "json_object"},
    )
    print("converted file " , os.path.basename(file_path))

    try:
        output = ConvertedFile(file_name=os.path.basename(file_path), python_code= Code.model_validate_json(chat_completion.choices[0].message.content).completePythonCode)
    except:
        print(chat_completion)
        output = json.loads(chat_completion.choices[0].message.content)
        if type(output , ConvertedFile):
            output = ConvertedFile(file_name=os.path.basename(file_path), python_code= Code.model_validate_json(chat_completion.choices[0].message.content).completePythonCode)
        else:
            return ConvertedFile(file_name=os.path.basename(file_path), python_code=output["properties"]["completePythonCode"]["value"])
# Main function to process files
def process_files(folder_path: str):
    """Process files in a folder: summarize, analyze dependencies, and convert."""
    # Step 1: Summarize all files
    files = [os.path.join(folder_path, f) for f in os.listdir(folder_path) if os.path.isfile(os.path.join(folder_path, f))]
    summaries = [summarize_file(file) for file in files]

    # Step 2: Analyze dependencies
    dependency_graph = analyze_dependencies(summaries)

    # Step 3: Traverse the graph and convert files
    sorted_files = list(nx.topological_sort(nx.DiGraph(dependency_graph.edges)))
    converted_files = []

    for file in sorted_files:
        # Collect context summaries of dependent files
        dependencies = dependency_graph.edges.get(file, [])
        context = "\n".join([summary.summary for summary in summaries if summary.file_name in dependencies])

        # Convert file
        print(f"Converting {file} with context from dependencies: {dependencies}")
        converted_file = get_recipe(os.path.join(folder_path, file), context)
        converted_files.append(converted_file)

    # Save converted files
    for converted in converted_files:
        output_file = os.path.join(folder_path, "converted", converted.file_name.replace(".cbl", ".py"))
        os.makedirs(os.path.dirname(output_file), exist_ok=True)  # Ensure the "converted" folder exists
        with open(output_file, 'w') as f:
            f.write(converted.python_code)
        print(f"Converted {converted.file_name} -> {output_file}")


# Example usage
if __name__ == "__main__":
    process_files("./cobol/files")
