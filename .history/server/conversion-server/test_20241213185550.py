import asyncio
from asyncio.log import logger
from datetime import datetime
import shutil
import tempfile
from fastapi import HTTPException
import json
import os
import subprocess
import networkx as nx
from typing import Any, AsyncGenerator, Dict, Generator, List
from groq import Groq
from pydantic import BaseModel
from pydantic import BaseModel, Field
from typing import Optional, Tuple
from pydantic import BaseModel
from langchain_google_genai import ChatGoogleGenerativeAI
from langchain.prompts import PromptTemplate
from langchain.output_parsers import PydanticOutputParser
from fastapi import FastAPI, UploadFile, File, Form
from fastapi.responses import JSONResponse, StreamingResponse
# Initialize Groq AI
groq = Groq(api_key="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")
from fastapi.middleware.cors import CORSMiddleware

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

async def stream_output(message: str):
    """
    Stream a message back to the client as a JSON event
    """
    yield f"data: {json.dumps({'message': message})}\n\n"

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

def improve_recipe(code: str, error: str, testCase: str, attempts: int) -> Generator[str, None, Code]:
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
    yield from stream_output(f"Improving the code with error {error} (Attempt {attempts}).... ")

    print(f"Improving the code with error {error} (Attempt {attempts}).... ")

    # Dynamic model selection based on attempts
    model_mapping = {
        2: "gemini-1.0-pro",
        3: "gemini-1.5-pro",
        4: "gemini-1.5-flash",
        5: "gemini-pro-1.5-latest"
    }
    
    # Default to the base model if attempts don't match the mapping
    selected_model = model_mapping.get(attempts, "gemini-1.5-flash-002")
    yield from stream_output(f"Selected model: {selected_model}")

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
    yield from stream_output(f"Code improvement process initiated...")

    improved_code = improvement_chain.invoke({
        "original_code": code,
        "error_message": error,
        "test_case": testCase,
        "attempt_number": attempts
    })
    yield from stream_output(f"Code improved successfully (Attempt {attempts}).")

    print(f"Code improved successfully (Attempt {attempts}).")
    return improved_code

def ai_agent(code_string: str, testCase: str, file_name: str, folder_path: str, max_attempts: int = 5) :
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
            yield from stream_output(f"Converted {file_name} -> {output_file}")
            yield from stream_output(f"Successful execution on attempt {attempt + 1} with result {result}")

            print(f"Converted {file_name} -> {output_file}")
            print(f"Successful execution on attempt {attempt + 1} with result {result}")
            return code_string
        else:
            # Attempt to improve the code
            yield from stream_output(f"Improvement attempt {attempt + 1}")

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
    print("Processing file " , file_path)
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
    os.makedirs(os.path.dirname("./cobol/summary/"), exist_ok=True)

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
app = FastAPI()
# Add CORS middleware
# Add CORS middleware to allow all origins, methods, and headers
app.add_middleware(
    CORSMiddleware,
    allow_origins=["*"],  # Allows requests from all origins
    allow_credentials=True,  # Allows credentials (e.g., cookies or auth headers)
    allow_methods=["*"],  # Allows all HTTP methods (GET, POST, etc.)
    allow_headers=["*"],  # Allows all headers in requests
)


# Main function to process files

async def stream_process_update(
    message: str,
    progress: Optional[float] = None,
    status: Optional[ProcessingStatus] = None
) -> str:
    """
    Create a formatted SSE message with optional progress and status.
    
    Args:
        message: Status message to send
        progress: Optional progress value between 0 and 1
        status: Optional processing status enum value
    
    Returns:
        Formatted SSE message string
    """
    event = ProcessingEvent(
        message=message,
        timestamp=datetime.now().isoformat(),
        progress=progress,
        status=status
    )
    return f"data: {event.model_dump_json()}\n\n"

async def ensure_folder_exists(folder_path: str) -> None:
    """Create folder if it doesn't exist."""
    Path(folder_path).mkdir(parents=True, exist_ok=True)

async def save_uploaded_file(file: UploadFile, folder_path: str) -> Path:
    """
    Save an uploaded file safely.
    
    Args:
        file: FastAPI UploadFile object
        folder_path: Destination folder path
    
    Returns:
        Path object for saved file
    
    Raises:
        IOError: If file saving fails
    """
    file_path = Path(folder_path) / file.filename
    try:
        async with aiofiles.open(file_path, 'wb') as f:
            content = await file.read()
            await f.write(content)
        return file_path
    except Exception as e:
        logger.error(f"Error saving file {file.filename}: {str(e)}")
        raise IOError(f"Failed to save file {file.filename}") from e
    finally:
        await file.close()

async def process_files_stream(
    folder_path: str,
    files: List[UploadFile]
) -> AsyncGenerator[str, None]:
    """
    Process files and yield status updates as server-sent events.
    
    Args:
        folder_path: Path to working directory
        files: List of uploaded files to process
    
    Yields:
        SSE formatted status messages
    """
    total_steps = 4
    current_step = 0
    
    try:
        # Ensure working directory exists
        await ensure_folder_exists(folder_path)
        
        # File upload phase
        yield await stream_process_update(
            "Starting file upload...",
            progress=current_step/total_steps,
            status=ProcessingStatus.UPLOADING
        )
        
        saved_files = []
        for file in files:
            try:
                file_path = await save_uploaded_file(file, folder_path)
                saved_files.append(file_path)
                yield await stream_process_update(
                    f"Uploaded: {file.filename}",
                    progress=(current_step + 0.2)/total_steps,
                    status=ProcessingStatus.UPLOADING
                )
            except IOError as e:
                yield await stream_process_update(
                    str(e),
                    status=ProcessingStatus.ERROR
                )
                continue
        
        current_step += 1
        
        # Summarization phase
        yield await stream_process_update(
            "Summarizing files...",
            progress=current_step/total_steps,
            status=ProcessingStatus.SUMMARIZING
        )
        
        summaries = []
        for idx, file_path in enumerate(saved_files):
            try:
                summary = await summarize_file(file_path)
                summaries.append(summary)
                yield await stream_process_update(
                    f"Summarized {file_path.name} ({idx + 1}/{len(saved_files)})",
                    progress=(current_step + (idx + 1)/len(saved_files))/total_steps,
                    status=ProcessingStatus.SUMMARIZING
                )
            except Exception as e:
                logger.error(f"Error summarizing {file_path}: {str(e)}")
                yield await stream_process_update(
                    f"Error summarizing {file_path.name}: {str(e)}",
                    status=ProcessingStatus.ERROR
                )
        
        current_step += 1
        
        # Dependency analysis phase
        yield await stream_process_update(
            "Analyzing dependencies...",
            progress=current_step/total_steps,
            status=ProcessingStatus.ANALYZING
        )
        
        try:
            dependency_graph = await analyze_dependencies(summaries)
            sorted_files = list(nx.topological_sort(dependency_graph))
            
            yield await stream_process_update(
                f"Found {len(sorted_files)} files in dependency order",
                progress=(current_step + 0.5)/total_steps,
                status=ProcessingStatus.ANALYZING
            )
        except nx.NetworkXUnfeasible:
            yield await stream_process_update(
                "Error: Circular dependency detected in files",
                status=ProcessingStatus.ERROR
            )
            return
        
        current_step += 1
        
        # Conversion phase
        converted_files: List[ConversionResult] = []
        for idx, file_path in enumerate(sorted_files):
            yield await stream_process_update(
                f"Converting {file_path.name}...",
                progress=(current_step + idx/len(sorted_files))/total_steps,
                status=ProcessingStatus.CONVERTING
            )
            
            try:
                # Get context from dependencies
                dependencies = dependency_graph.predecessors(file_path)
                context = "\n".join(
                    summary.summary for summary in summaries 
                    if summary.file_path in dependencies
                )
                
                # Convert file
                result = await convert_cobol_to_python(file_path, context)
                
                # Test converted code
                yield await stream_process_update(
                    f"Testing converted code for {file_path.name}...",
                    progress=(current_step + (idx + 0.5)/len(sorted_files))/total_steps,
                    status=ProcessingStatus.TESTING
                )
                
                await test_converted_code(
                    result.converted_code,
                    result.test_code,
                    file_path,
                    folder_path
                )
                
                converted_files.append(result)
                
            except Exception as e:
                logger.error(f"Error processing {file_path}: {str(e)}")
                yield await stream_process_update(
                    f"Error processing {file_path.name}: {str(e)}",
                    status=ProcessingStatus.ERROR
                )
        
        # Final success message
        yield await stream_process_update(
            f"Successfully processed {len(converted_files)} files",
            progress=1.0,
            status=ProcessingStatus.COMPLETED
        )
        
    except Exception as e:
        logger.error(f"Unexpected error: {str(e)}")
        yield await stream_process_update(
            f"Unexpected error: {str(e)}",
            status=ProcessingStatus.ERROR
        )

@app.post("/process", 
    summary="Process COBOL files",
    response_class=StreamingResponse,
    responses={
        200: {"description": "Stream of processing events"},
        400: {"description": "Invalid request"},
        500: {"description": "Internal server error"}
    }
)
async def process_files(
    folder_path: str = Form(..., description="Working directory path"),
    files: List[UploadFile] = File(..., description="COBOL files to process")
) -> StreamingResponse:
    """
    Process uploaded COBOL files and stream progress updates.
    
    Args:
        folder_path: Working directory path
        files: List of COBOL files to process
    
    Returns:
        StreamingResponse with SSE updates
    
    Raises:
        HTTPException: If request is invalid
    """
    if not files:
        raise HTTPException(status_code=400, detail="No files provided")
    
    return StreamingResponse(
        process_files_stream(folder_path, files),
        media_type="text/event-stream"
    )

@app.get("/", 
    summary="Root endpoint",
    response_model=dict
)
async def root() -> Dict[str, str]:
    """Root endpoint returning welcome message."""
    return {"message": "Welcome to the COBOL file processing server!"}

if __name__ == "__main__":
    import uvicorn
    uvicorn.run(
        app,
        host="0.0.0.0",
        port=8000,
        log_level="info"
    )