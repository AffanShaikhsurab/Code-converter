from asyncio.log import logger
import tempfile
from fastapi import HTTPException
import json
import os
import subprocess
import networkx as nx
from typing import Any, Dict, List
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

import os
import sys
import subprocess
import webbrowser
import networkx as nx
from typing import Dict, List, Optional

# Additional imports for GitHub integration
import requests
from github import Github, GithubException
from github.Repository import Repository
from github.ContentFile import ContentFile

# Keep existing imports from the original script
from asyncio.log import logger
from fastapi import HTTPException
from groq import Groq
from pydantic import BaseModel, Field
from langchain_google_genai import ChatGoogleGenerativeAI
from langchain.prompts import PromptTemplate
from langchain.output_parsers import PydanticOutputParser

from langchain.tools import BaseTool
from langchain.pydantic_v1 import BaseModel, Field
from langchain_core.utils.function_calling import convert_to_openai_function

class FileWriterInput(BaseModel):
    """Input model for file writing tool."""
    content: str = Field(..., description="Content to write to the file")
    filename: str = Field(..., description="Name of the file to create")
    directory: Optional[str] = Field(default=None, description="Optional directory path")

class FileWriterTool(BaseTool):
    """Tool to write content to a file."""
    name: str = "write_file"  # Added type annotation
    description: str = "Write content to a file, optionally specifying a directory"
    args_schema: type[BaseModel] = FileWriterInput

    def _run(self, content: str, filename: str, directory: Optional[str] = None) -> str:
        """
        Write content to a file.
        
        Args:
            content (str): Content to write to the file
            filename (str): Name of the file
            directory (Optional[str]): Directory to save the file in
        
        Returns:
            str: Path to the created file
        """
        try:
            # Use provided directory or default to current working directory
            target_dir = directory or os.getcwd()
            
            # Ensure directory exists
            os.makedirs(target_dir, exist_ok=True)
            
            # Full file path
            file_path = os.path.join(target_dir, filename)
            
            # Write content to file
            with open(file_path, 'w') as f:
                f.write(content)
            
            return f"File successfully written to {file_path}"
        except Exception as e:
            return f"Error writing file: {str(e)}"
# GitHub OAuth Configuration

GITHUB_CLIENT_ID = 'Ov23liUyXsesP4nHuerk'  # Replace with your GitHub OAuth App Client ID
GITHUB_CLIENT_SECRET = '1a12db1d38254b29284f9752e57374aa6936e789'  # Replace with your GitHub OAuth App Client Secret
GITHUB_REDIRECT_URI = 'http://localhost:8000/callback'

class GitHubAuthorizer:
    def __init__(self):
        self.access_token = None

    def authorize(self):
        """
        Initiate GitHub OAuth authorization flow.
        Opens browser for user to authorize the application.
        """
        # Generate authorization URL
        auth_url = (
            f"https://github.com/login/oauth/authorize"
            f"?client_id={GITHUB_CLIENT_ID}"
            f"&redirect_uri={GITHUB_REDIRECT_URI}"
            f"&scope=repo"
        )
        
        print("Opening browser for GitHub authorization...")
        webbrowser.open(auth_url)

        # Prompt user to enter the code
        print("\nAfter authorizing, GitHub will redirect you to a page with a code.")
        print("Please copy the entire URL from the redirect page and paste it here:")
        redirect_url = input("Redirect URL: ")

        # Extract code from the redirect URL
        try:
            code = redirect_url.split('code=')[1].split('&')[0]
        except IndexError:
            print("Invalid redirect URL. Authorization failed.")
            return None

        # Exchange code for access token
        token_response = requests.post(
            'https://github.com/login/oauth/access_token',
            data={
                'client_id': GITHUB_CLIENT_ID,
                'client_secret': GITHUB_CLIENT_SECRET,
                'code': code,
                'redirect_uri': GITHUB_REDIRECT_URI
            },
            headers={'Accept': 'application/json'}
        )

        token_data = token_response.json()
        self.access_token = token_data.get('access_token')

        if not self.access_token:
            print("Failed to obtain access token.")
            return None

        return self.access_token

    def list_repositories(self):
        """
        List user's repositories after authorization.
        """
        if not self.access_token:
            print("Please authorize first.")
            return []

        g = Github(self.access_token)
        try:
            repos = list(g.get_user().get_repos())
            print("Your Repositories:")
            for i, repo in enumerate(repos, 1):
                print(f"{i}. {repo.full_name}")
            return repos
        except GithubException as e:
            print(f"Error fetching repositories: {e}")
            return []

    def select_repository(self):
        """
        Allow user to select a repository interactively.
        """
        repos = self.list_repositories()
        if not repos:
            return None

        while True:
            try:
                selection = int(input("Enter the number of the repository to convert: "))
                if 1 <= selection <= len(repos):
                    return repos[selection - 1]
            except ValueError:
                print("Please enter a valid number.")

    def list_cobol_folders_in_repo(self, repo: Repository):
        """
        List folders containing COBOL files in a repository.
        """
        cobol_folders = []
        
        def find_cobol_folders(contents, current_path=''):
            for content in contents:
                if content.type == 'dir':
                    try:
                        # Check if the directory contains COBOL files
                        dir_contents = repo.get_contents(content.path)
                        has_cobol_files = any(
                            file.name.lower().endswith(('.cbl', '.cobol')) 
                            for file in dir_contents if file.type == 'file'
                        )
                        
                        if has_cobol_files:
                            cobol_folders.append(content.path)
                        
                        # Recursively check subdirectories
                        find_cobol_folders(dir_contents, content.path)
                        
                    except Exception as e:
                        print(f"Could not access directory {content.path}: {e}")

        try:
            contents = repo.get_contents('')
            find_cobol_folders(contents)
            
            print("COBOL Folders Found:")
            for i, folder in enumerate(cobol_folders, 1):
                print(f"{i}. {folder}")
            
            return cobol_folders
        except GithubException as e:
            print(f"Error finding COBOL folders: {e}")
            return []

    def select_cobol_folder(self):
        """
        Allow user to select a folder containing COBOL files interactively.
        """
        cobol_folders = self.list_cobol_folders_in_repo(self.repo)
        if not cobol_folders:
            return None

        while True:
            try:
                selection = int(input("Enter the number of the folder to convert: "))
                if 1 <= selection <= len(cobol_folders):
                    return cobol_folders[selection - 1]
            except ValueError:
                print("Please enter a valid number.")



    def download_cobol_folder(self, repo: Repository, folder_path: str, output_folder: str):
        """
        Download all COBOL files from a specific folder in the repository.
        
        Args:
            repo (Repository): GitHub repository object
            folder_path (str): Path to the folder containing COBOL files
            output_folder (str): Local output directory to save files
        """
        os.makedirs(output_folder, exist_ok=True)
        
        try:
            # Ensure folder_path is a string, not a list
            if isinstance(folder_path, list):
                folder_path = folder_path[0]
            
            # Get contents of the specified folder
            folder_contents = repo.get_contents(folder_path)
            
            # Recursively download COBOL files
            def download_recursive(contents, current_path=''):
                for content in contents:
                    if content.type == 'file' and content.name.lower().endswith(('.cbl', '.cobol')):
                        try:
                            # Create local file path, preserving subfolder structure if needed
                            local_file_path = os.path.join(output_folder, current_path, content.name)
                            os.makedirs(os.path.dirname(local_file_path), exist_ok=True)
                            
                            with open(local_file_path, 'wb') as f:
                                f.write(content.decoded_content)
                            
                            print(f"Downloaded: {content.path} -> {local_file_path}")
                        except Exception as e:
                            print(f"Error downloading {content.path}: {e}")
                    
                    # Recursively handle subdirectories
                    elif content.type == 'dir':
                        try:
                            sub_contents = repo.get_contents(content.path)
                            download_recursive(sub_contents, os.path.join(current_path, content.name))
                        except Exception as e:
                            print(f"Could not access subdirectory {content.path}: {e}")
            
            # Start recursive download
            download_recursive(folder_contents)
        
        except Exception as e:
            print(f"Error accessing folder {folder_path}: {e}")
            # Print more detailed error information
            import traceback
            traceback.print_exc()

    def list_cobol_folders_in_repo(self, repo: Repository):
        """
        List folders containing COBOL files in a repository.
        """
        cobol_folders = []
        
        def find_cobol_folders(contents, current_path=''):
            for content in contents:
                if content.type == 'dir':
                    try:
                        # Check if the directory contains COBOL files
                        dir_contents = repo.get_contents(content.path)
                        has_cobol_files = any(
                            file.name.lower().endswith(('.cbl', '.cobol')) 
                            for file in dir_contents if file.type == 'file'
                        )
                        
                        # If directory contains COBOL files or has subdirectories with COBOL files
                        if has_cobol_files or any(
                            subfile.type == 'file' and subfile.name.lower().endswith(('.cbl', '.cobol'))
                            for subfile in dir_contents if subfile.type == 'dir'
                        ):
                            cobol_folders.append(content.path)
                        
                        # Recursively check subdirectories
                        find_cobol_folders(dir_contents, content.path)
                        
                    except Exception as e:
                        print(f"Could not access directory {content.path}: {e}")

        try:
            contents = repo.get_contents('')
            find_cobol_folders(contents)
            
            print("COBOL Folders Found:")
            for i, folder in enumerate(cobol_folders, 1):
                print(f"{i}. {folder}")
            
            return cobol_folders
        except GithubException as e:
            print(f"Error finding COBOL folders: {e}")
            return []

    def select_cobol_folder(self):
        """
        Allow user to select a folder containing COBOL files interactively.
        """
        cobol_folders = self.list_cobol_folders_in_repo(self.repo)
        if not cobol_folders:
            return None

        while True:
            try:
                selection = int(input("Enter the number of the folder to convert: "))
                if 1 <= selection <= len(cobol_folders):
                    return cobol_folders[selection - 1]
            except ValueError:
                print("Please enter a valid number.")

def github_cobol_conversion_workflow():
    """
    Main workflow for GitHub COBOL to Python conversion.
    """
    # GitHub Authorization
    authorizer = GitHubAuthorizer()
    
    # Step 1: Authorize GitHub access
    print("Starting GitHub Authorization...")
    access_token = authorizer.authorize()
    if not access_token:
        print("Authorization failed. Exiting.")
        return

    # Step 2: Select Repository
    print("\nSelect a Repository to Convert COBOL Files...")
    repo = authorizer.select_repository()
    if not repo:
        print("No repository selected. Exiting.")
        return

    # Step 3: Find COBOL Folders
    print(f"\nSearching for folders with COBOL files in {repo.full_name}...")
    authorizer.repo = repo  # Add repo as an attribute for folder selection
    cobol_folders = authorizer.list_cobol_folders_in_repo(repo)
    if not cobol_folders:
        print("No COBOL folders found. Exiting.")
        return

    # Step 4: Select COBOL Folder
    print("\nSelect a COBOL folder to convert:")
    selected_folder = authorizer.select_cobol_folder()
    if not selected_folder:
        print("No folder selected. Exiting.")
        return

    # Step 5: Download Folder Contents
    local_cobol_folder = "./cobol/files"
    os.makedirs(local_cobol_folder, exist_ok=True)
    authorizer.download_cobol_folder(repo, selected_folder, local_cobol_folder)

    # Step 6: Run COBOL to Python Conversion
    print("\nStarting COBOL to Python Conversion...")
        
    process_files(local_cobol_folder)
    
    
    
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
        5. Generate a comprehensive, meaningful test case , mock the inputs dont take from user 
        
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
    
    


def run_code(
    code: str, 
    testCase: str, 
    timeout: int = 30, 
    input_value: Optional[Any] = None,
    file_tools: Optional[list[BaseTool]] = None
) -> Tuple[bool, str]:
    """
    Execute Python code with enhanced capabilities using Langchain tools.
    
    Args:
        code (str): Python code to execute
        testCase (str): Test case to validate the code
        timeout (int, optional): Maximum execution time in seconds. Defaults to 30.
        input_value (Optional[Any]): Optional input value to pass to the code
        file_tools (Optional[list[BaseTool]]): Optional Langchain tools for file operations
    
    Returns:
        Tuple[bool, str]: Success status and result/error message
    """
    # Prepare tools
    if file_tools is None:
        file_tools = [FileWriterTool()]
    
    # Clean up test case and code
    test_code = code + "\n" + testCase.replace("```python\n", "").replace("```python", "").replace("```", "")
    
    # Optionally inject input handling
    if input_value is not None:
        input_injection = f"""
# Inject input value for testing
input_value = {repr(input_value)}
"""
        test_code = input_injection + test_code
    
    try:
        # Use a temporary file to execute the code safely
        with tempfile.NamedTemporaryFile(mode='w', suffix='.py', delete=False) as temp_file:
            temp_filename = temp_file.name
            temp_file.write(test_code)
            temp_file.flush()
        
        try:
            # Execute the code with timeout and capture output
            result = subprocess.run(
                [sys.executable, temp_filename], 
                capture_output=True, 
                text=True,
                timeout=timeout
            )

            # Determine success based on return code
            if result.returncode != 0:
                error_message = result.stderr or result.stdout
                print("Execution Error:", error_message)
                return False, error_message
            
            # Successful execution
            output = result.stdout.strip()
            print("Execution Output:", output)
            
            # Return execution result with potentially injected tools
            tool_descriptions = "\nAvailable Tools:\n" + "\n".join([
                f"- {tool.name}: {tool.description}" for tool in file_tools
            ])
            
            return True, output + tool_descriptions

        except subprocess.TimeoutExpired:
            print(f"Execution timed out after {timeout} seconds")
            return False, f"Execution timed out after {timeout} seconds"
        
        except Exception as exec_error:
            print(f"Execution failed: {exec_error}")
            return False, str(exec_error)
        
        finally:
            # Always clean up the temporary file
            try:
                os.unlink(temp_filename)
            except Exception as cleanup_error:
                print(f"Error cleaning up temp file: {cleanup_error}")

    except Exception as file_error:
        print(f"File preparation error: {file_error}")
        return False, str(file_error)

def improve_recipe(code: str, error: str, testCase: str, attempts: int) -> Code:
    """
    Improve the code based on the error encountered with dynamic model selection.
    
    Args:
        code (str): Original Python code
        error (str): Error message from code execution
        testCase (str): Original test case
        attempts (int): Number of improvement attempts
    
    Returns:
        Code: Improved code with updated implementation
    """
    print(f"Improving the code with error {error} (Attempt {attempts}).... ")

    # Dynamic model selection based on attempts
    model_mapping = {
        2: "gemini-1.0-pro",
        3: "gemini-1.5-pro",
        3: "gemini-1.5-flash",
        5: "gemini-pro-1.5-latest"
    }
    
    # Default to the base model if attempts don't match the mapping
    selected_model = model_mapping.get(attempts, "gemini-1.5-flash-002")
    
    print(f"Selected model: {selected_model}")

    # Use the dynamically selected model with context about the error
    llm = ChatGoogleGenerativeAI(
        model=selected_model, 
        google_api_key=GEMINI_API_KEY, 
        temperature=0.7
    )
    
    code_parser = PydanticOutputParser(pydantic_object=Code)
    
    improvement_prompt_template = PromptTemplate(
        template="""
        Code Improvement Request:
        
        Original Code:
        {original_code}
        
        Error Encountered:
        {error_message}
        
        Test Case:
        {test_case}
        
        Improvement Attempt Number:
        {attempt_number}
        
        Improvement Requirements:
        1. Carefully analyze the error and original code
        2. Generate a significantly improved Python implementation
        3. Ensure the code passes the test case (mock inputs, do not rely on user input)
        4. Apply advanced Python best practices and optimization techniques
        5. Provide clear, concise, and efficient solution
        
        Output Format:
        {format_instructions}
        """,
        input_variables=["original_code", "error_message", "test_case", "attempt_number"],
        partial_variables={
            "format_instructions": code_parser.get_format_instructions()
        }
    )
    
    improvement_chain = improvement_prompt_template | llm | code_parser
    print("Code improvement process initiated...")
    
    improved_code = improvement_chain.invoke({
        "original_code": code,
        "error_message": error,
        "test_case": testCase,
        "attempt_number": attempts
    })
    
    print(f"Code improved successfully (Attempt {attempts}).")
    return improved_code

def ai_agent(code_string: str, testCase: str, file_name: str, folder_path: str, max_attempts: int = 3) -> str:
    """
    Agent to execute code, save successful implementation, or save for manual review if max attempts reached.
    
    Args:
        code_string (str): Python code to execute
        testCase (str): Test case to validate the code
        file_name (str): Name of the original file
        folder_path (str): Base folder path for saving files
        max_attempts (int, optional): Maximum improvement attempts. Defaults to 3
    
    Returns:
        str: Final executed code
    """
    for attempt in range(max_attempts):
        success, result = run_code(code_string, testCase)
        
        if success:
            # Save successful code 
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
                improved_code = improve_recipe(code_string, result, testCase , attempt + 1)
                code_string = improved_code.completePythonCode
            except Exception as e:
                print(f"Improvement failed: {e}")
                break
    
    # When max attempts are reached, save code to manual review folder
    manual_review_path = os.path.join(folder_path, "manual_review")
    os.makedirs(manual_review_path, exist_ok=True)
    
    manual_review_file = os.path.join(manual_review_path, file_name.replace(".cbl", ".py"))
    with open(manual_review_file, 'w') as f:
        f.write(code_string)
    
    print(f"Maximum improvement attempts reached. Code saved for manual review at {manual_review_file}")
    
    return code_string
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
    print("Dependency Graph Edges:")
    for edge in dependency_graph.edges:
        print(f"  {edge[0]} -> {edge[1]}")
        
    sorted_files = list(nx.topological_sort(nx.DiGraph(dependency_graph.edges)))
    converted_files : list[Code]= []

    for file in sorted_files:
        # Collect context summaries of dependent files
        dependencies = dependency_graph.edges.get(file, [])
        context = "\n".join([summary.summary for summary in summaries if summary.file_name in dependencies])

        # Convert file
        print(f"Converting {file} with context from dependencies: {dependencies}")
        converted_file : Code = convert_cobol_to_python(os.path.join(folder_path, file), context)
        print("Using ai agent to check the code execution ......")
        parsed_code = converted_file.completePythonCode.replace("```python\n", "").replace("```python", "").replace("```" , "")
        parsed_test  = converted_file.singleTestAssertLine.replace("```python\n", "").replace("```python", "").replace("```" , "")
        ai_agent(parsed_code , parsed_test , file , folder_path)
        converted_files.append(converted_file)


# Example usage
if __name__ == "__main__":
    github_cobol_conversion_workflow()
