import os
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


# Summarize each file
def summarize_file(file_path: str) -> FileSummary:
    """Summarize the content of a COBOL file using Groq."""
    with open(file_path, 'r') as file:
        content = file.read()

    response = groq.chat.completions.create(
        messages=[
            {"role": "system", "content": "You are a COBOL summarizer. Summarize the following COBOL code:"},
            {"role": "user", "content": content},
        ],
        model="mixtral-8x7b-32768",
        temperature=0,
    )

    summary = response.choices[0].message.content.strip()
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


# Convert COBOL file to Python
def convert_file(file_path: str, context: str) -> ConvertedFile:
    """Convert a COBOL file to Python using Groq and provided context."""
    with open(file_path, 'r') as file:
        content = file.read()

    response = groq.chat.completions.create(
        messages=[
            {"role": "system", "content": "You are a COBOL-to-Python code converter."},
            {"role": "user", "content": f"Context: {context}\nCode to convert:\n{content}"},
        ],
        model="mixtral-8x7b-32768",
        temperature=0,
    )

    python_code = response.choices[0].message.content.strip()
    return ConvertedFile(file_name=os.path.basename(file_path), python_code=python_code)


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
        converted_file = convert_file(os.path.join(folder_path, file), context)
        converted_files.append(converted_file)

    # Save converted files
    for converted in converted_files:
        output_file = os.path.join("./cobol/converted", converted.file_name.replace(".cbl", ".py"))
        with open(output_file, 'w') as f:
            f.write(converted.python_code)
        print(f"Converted {converted.file_name} -> {output_file}")


# Example usage
if __name__ == "__main__":
    process_files("./cobol/files")
