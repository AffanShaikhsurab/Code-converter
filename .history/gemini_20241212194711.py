from asyncio.log import logger
from http.client import HTTPException
import os
import subprocess
from langchain_google_genai import ChatGoogleGenerativeAI
from pydantic import BaseModel, Field
from typing import Optional
from pydantic import BaseModel
from langchain_google_genai import ChatGoogleGenerativeAI
from langchain.prompts import PromptTemplate
from langchain.output_parsers import PydanticOutputParser
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
    
    improved_code = improvement_chain.invoke({
        "original_code": code,
        "error_message": error,
        "test_case": testCase
    })
    
    return improved_code

def ai_agent(code_string: str, testCase: str, max_attempts: int = 3) -> str:
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
            print(f"Successful execution on attempt {attempt + 1}")
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

# Example usage
def main():
    # Example COBOL code
    example_cobol = """
       IDENTIFICATION DIVISION.
       PROGRAM-ID. SIMPLE-CALCULATOR.
       ENVIRONMENT DIVISION.
       DATA DIVISION.
       WORKING-STORAGE SECTION.
       01 NUM1 PIC 9(5).
       01 NUM2 PIC 9(5).
       01 RESULT PIC 9(10).
       PROCEDURE DIVISION.
           MOVE 10 TO NUM1.
           MOVE 20 TO NUM2.
           COMPUTE RESULT = NUM1 + NUM2.
           DISPLAY RESULT.
           STOP RUN.
    """
    
    # Convert and execute COBOL code
    final_python_code = convert_cobol_to_python_with_execution(example_cobol)
    print("Final Executed Python Code:")
    print(final_python_code)

if __name__ == "__main__":
    main()