from asyncio.log import logger
from http.client import HTTPException
import json
import os
import subprocess
import networkx as nx
from typing import Dict, List
from groq import Groq
from pydantic import BaseModel
from pydantic import BaseModel, Field
from typing import Optional, Tuple
from pydantic import BaseModel
from langchain_google_genai import ChatGoogleGenerativeAI
from langchain.prompts import PromptTemplate
from langchain.output_parsers import PydanticOutputParser

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


GEMINI_API_KEY = os.getenv("GEMINI_API_KEY", "AIzaSyAuEyEPkQv5_Fc8o2bLrhPLv_XTxFeUv3o")
def saveFile(filename: str, code: str, extension: str = ".py"):
    """
    Save code to a file with optional extension.
    
    Args:
        filename (str): Base filename
        code (str): Code content to save
        extension (str, optional): File extension. Defaults to .py
    """
    full_filename = f"{filename}{extension}"
    with open(full_filename, 'w') as file:
        file.write(code)
    print(f"File saved: {full_filename}")
    return full_filename


from pydantic import BaseModel, Field
from typing import Optional

class Code(BaseModel):
    completePythonCode: str = Field(..., description="Full converted Python code")
    singleTestAssertLine: str = Field(default=None, description="Single line test case assertion")


def convert_cobol_to_python(
    cobol_source_code: str, 
    context: Optional[str] = None, 
    max_retries: int = 3
) -> Code:
    """
    Convert COBOL code to Python using advanced language model conversion.
    
    Args:
        cobol_source_code (str): The original COBOL source code to convert
        context (Optional[str]): Additional context about dependent files or system
        max_retries (int): Maximum number of conversion attempts
    
    Returns:
        Code: Pydantic model with converted Python code and test assertion
    
    Raises:
        HTTPException: For input validation or conversion errors
    """
    # Input validation with more comprehensive checks
    if not isinstance(cobol_source_code, str):
        raise ValueError("COBOL source code must be a string")
    
    clean_source_code = cobol_source_code.strip()
    if not clean_source_code:
        raise ValueError("COBOL source code cannot be empty")
    
    # Secure API key handling (assumed to be implemented securely)
    try:
        llm = ChatGoogleGenerativeAI(
            model="gemini-1.5-flash-002", 
            google_api_key=GEMINI_API_KEY, 
            temperature=0.7,
            max_tokens=4096  # Prevent excessive token generation
        )
    except Exception as api_error:
        logger.error(f"API initialization error: {api_error}")
        raise HTTPException(
            status_code=500, 
            detail="Failed to initialize language model"
        )
    
    # Enhanced Prompt Template with More Context
    code_parser = PydanticOutputParser(pydantic_object=Code)
    
    conversion_prompt_template = PromptTemplate(
        template="""
        COBOL to Python Conversion Detailed Task:
        
        Context Information:
        {context_info}
        
        Source COBOL Code:
        ```cobol
        {cobol_code}
        ```
        
        Comprehensive Conversion Guidelines:
        Note: The context information, summarizing the files that the cobol_code depends on, is provided to assist in the conversion process.
        1. Translate COBOL code to idiomatic, modern Python
        2. Preserve original algorithmic logic precisely
        3. Handle potential data type conversions
        4. Follow Python PEP 8 style guidelines
        5. Generate a comprehensive, meaningful test case
        
        Output Specifications:
        {format_instructions}
        
        Conversion Expectations:
        - Produce fully executable Python code
        - Include type hints and docstrings
        - Create a robust test assertion
        - Provide clear comments explaining translation decisions
        """,
        input_variables=["cobol_code", "context_info"],
        partial_variables={
            "format_instructions": code_parser.get_format_instructions(),
            "context_info": context or "No additional context provided"
        }
    )
    
    # Retry Mechanism for Conversion
    for attempt in range(max_retries):
        try:
            # Conversion Chain with Enhanced Error Handling
            conversion_chain = conversion_prompt_template | llm | code_parser
            
            converted_code : Code = conversion_chain.invoke({
                "cobol_code": clean_source_code,
                "context_info": context or ""
            })
            
            # Validate conversion output
            if not converted_code.completePythonCode or not converted_code.singleTestAssertLine:
                raise ValueError("Incomplete conversion result")
            
            return converted_code
        
        except Exception as conversion_error:
            logger.warning(
                f"Conversion attempt {attempt + 1} failed: {conversion_error}"
            )
            
            # Final attempt handling
            if attempt == max_retries - 1:
                logger.error(f"Conversion failed after {max_retries} attempts")
                raise HTTPException(
                    status_code=500, 
                    detail="Unable to convert COBOL code to Python"
                )
    
    # Unreachable, but added for type safety
    raise HTTPException(
        status_code=500, 
        detail="Unexpected conversion failure"
    )
    
    
def run_code(code: str, testCase: str) -> Tuple[bool, str]:
    """
    Execute Python code with a test case.
    
    Args:
        code (str): Python code to execute
        testCase (str): Test case to validate the code
    
    Returns:
        Tuple[bool, str]: Success status and result/error message
    """
    try:
        # Combine code and test case
        test_code = code + "\n" + testCase.replace("```python\n", "").replace("```python", "")
        test_code = test_code.replace("```", "")
        print("testing the code .... ")
        # Execute the combined code
        result = subprocess.run(
            ["python", "-c", test_code], 
            capture_output=True, 
            text=True
        )

        # Check execution result
        if result.returncode != 0:
            error_message = result.stderr
            print("Error message:", error_message)
            return False, error_message
        else:
            output = result.stdout
            print("Execution output:", output)
            return True, output.strip()

    except Exception as e:
        return False, str(e)

def improve_recipe(code: str, error: str, testCase: str) -> Code:
    """
    Improve the code based on the error encountered.
    
    Args:
        code (str): Original Python code
        error (str): Error message from code execution
        testCase (str): Original test case
    
    Returns:
        Code: Improved code with updated implementation
    """
    print(f"imporve  the code with error {error} .... ")

    # Use the existing conversion function, but provide context about the error
    llm = ChatGoogleGenerativeAI(model="gemini-1.5-flash-002", google_api_key=GEMINI_API_KEY, temperature=0.7)
    
    code_parser = PydanticOutputParser(pydantic_object=Code)
    
    improvement_prompt_template = PromptTemplate(
        template="""
        Code Improvement Request:
        
        Original Code:
        ```python
        {original_code}
        ```
        
        Error Encountered:
        {error_message}
        
        Test Case:
        {test_case}
        
        Improvement Requirements:
        1. Analyze the error and original code
        2. Generate an improved Python implementation
        3. Ensure the code passes the test case
        4. Provide a corrected version following Python best practices
        
        Output Format:
        {format_instructions}
        """,
        input_variables=["original_code", "error_message", "test_case"],
        partial_variables={
            "format_instructions": code_parser.get_format_instructions()
        }
    )
    
    improvement_chain = improvement_prompt_template | llm | code_parser
    print("code imporved .. ")
    improved_code = improvement_chain.invoke({
        "original_code": code,
        "error_message": error,
        "test_case": testCase
    })
    
    return improved_code

def ai_agent(code_string: str, testCase: str, file_name : str , folder_path : str ,  max_attempts: int = 3  ) -> str:
    """
    Agent to execute code, save successful implementation, and improve if needed.
    
    Args:
        code_string (str): Python code to execute
        testCase (str): Test case to validate the code
        max_attempts (int, optional): Maximum improvement attempts. Defaults to 3
    
    Returns:
        str: Final executed code
    """
    for attempt in range(max_attempts):
        success, result = run_code(code_string, testCase)
        
        if success:
            # Save successful code and return
            saveFile("ai_agent_created_code", code_string)
            output_file = os.path.join(folder_path, "converted", file_name.replace(".cbl", ".py"))
            os.makedirs(os.path.dirname(output_file), exist_ok=True)  # Ensure the "converted" folder exists
            with open(output_file, 'w') as f:
                f.write(code_string)
            print(f"Converted {file_name} -> {output_file}")
            print(f"Successful execution on attempt {attempt + 1} with result {result}")
            return code_string
        else:
            # Attempt to improve the code
            print(f"Improvement attempt {attempt + 1}")
            try:
                improved_code = improve_recipe(code_string, result, testCase)
                code_string = improved_code.completePythonCode
            except Exception as e:
                print(f"Improvement failed: {e}")
                break
    
    print("Maximum improvement attempts reached. Code execution failed.")
    return code_string

def convert_cobol_to_python_with_execution(cobol_source_code: str, context: str = "") -> str:
    """
    Convert COBOL to Python and execute the code.
    
    Args:
        cobol_source_code (str): COBOL source code to convert
        context (str, optional): Additional context for conversion
    
    Returns:
        str: Executed Python code
    """
    # Convert COBOL to Python
    converted_code = convert_cobol_to_python(cobol_source_code)
    
    # Execute the converted code
    final_code = ai_agent(
        converted_code.completePythonCode, 
        converted_code.singleTestAssertLine
    )
    return final_code

    
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


# Main function to process files
def process_files(folder_path: str):
    """Process files in a folder: summarize, analyze dependencies, and convert."""
    # Step 1: Summarize all files
    files = [os.path.join(folder_path, f) for f in os.listdir(folder_path) if os.path.isfile(os.path.join(folder_path, f))]
    print("Using ai agent to summarize files ......")

    summaries = [summarize_file(file)  for file in files]

    # Step 2: Analyze dependencies
    print("Using ai agent to analyze dependencies ......")

    dependency_graph = analyze_dependencies(summaries)

    # Step 3: Traverse the graph and convert files
    print("Using ai agent to create DiGraph ......")

    sorted_files = list(nx.topological_sort(nx.DiGraph(dependency_graph.edges)))
    converted_files : list[Code]= []

    for file in sorted_files:
        # Collect context summaries of dependent files
        dependencies = dependency_graph.edges.get(file, [])
        context = "\n".join([summary.summary for summary in summaries if summary.file_name in dependencies])

        # Convert file
        print(f"Converting {file} with context from dependencies: {dependencies}")
        converted_file : Code = convert_cobol_to_python(os.path.join(folder_path, file), context)
        converted_files.append(converted_file)

    # Save converted files
    for converted  in converted_files:
        print("Using ai agent to check the code execution ......")
        ai_agent(converted.completePythonCode , converted.singleTestAssertLine , converted.file_name , folder_path)



# Example usage
if __name__ == "__main__":
    process_files("./cobol/files")
