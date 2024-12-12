from pydantic import BaseModel, Field
from typing import Optional

class Code(BaseModel):
    completePythonCode: str = Field(..., description="Full converted Python code")
    singleTestAssertLine: Optional[str] = Field(default=None, description="Single line test case assertion")

def convert_cobol_to_python(cobol_source_code: str) -> Code:
    """
    Convert COBOL code to Python using Pydantic-validated output.
    
    Args:
        cobol_source_code (str): The original COBOL source code to convert
    
    Returns:
        Code: Pydantic model with converted Python code and test assertion
    """
    try:
        # Validate input
        if not cobol_source_code or len(cobol_source_code.strip()) == 0:
            raise ValueError("No COBOL source code provided for conversion")
        
        # Initialize language model for conversion
        llm = ChatGoogleGenerativeAI(model="gemini-1.5-flash-002", google_api_key=GEMINI_API_KEY, temperature=0.7)
        
        # Conversion Prompt Template with Pydantic Parser
        code_parser = PydanticOutputParser(pydantic_object=Code)
        
        conversion_prompt_template = PromptTemplate(
            template="""
            COBOL to Python Conversion Task:
            
            Source COBOL Code:
            ```cobol
            {cobol_code}
            ```
            
            Conversion Requirements:
            1. Translate COBOL code to standard Python
            2. Preserve original program logic
            3. Generate a single meaningful test case assertion
            
            Output Format:
            {format_instructions}
            
            Specific Tasks:
            - Provide complete, runnable Python code
            - Create a simple test assertion that validates a key aspect of the code
            - Ensure code follows Python best practices
            """,
            input_variables=["cobol_code"],
            partial_variables={
                "format_instructions": code_parser.get_format_instructions()
            }
        )
        
        # Conversion Chain with Pydantic Parser
        conversion_chain = conversion_prompt_template | llm | code_parser
        
        # Generate Converted Python Code
        converted_code = conversion_chain.invoke({
            "cobol_code": cobol_source_code
        })
        
        return converted_code
    
    except ValueError as e:
        logger.error(f"Conversion preparation error: {e}")
        raise HTTPException(status_code=400, detail=str(e))
    except Exception as e:
        logger.error(f"COBOL to Python conversion error: {e}")
        raise HTTPException(status_code=500, detail="Conversion process failed")

# Example usage function
def demonstrate_conversion():
    """
    Demonstrate the COBOL to Python conversion with an example.
    """
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
    
    try:
        # Convert COBOL to Python
        converted_code = convert_cobol_to_python(example_cobol)
        
        # Print results
        print("Converted Python Code:")
        print(converted_code.completePythonCode)
        
        print("\nTest Assertion:")
        print(converted_code.singleTestAssertLine)
    
    except Exception as e:
        print(f"Conversion error: {e}")

# Optional: Run demonstration
if __name__ == "__main__":
    demonstrate_conversion()