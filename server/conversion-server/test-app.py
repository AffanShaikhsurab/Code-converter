import os
import shutil
import requests

def test_process_files_with_streaming():
    folder_path = "test_uploads"

    # Ensure the test folder is clean
    if os.path.exists(folder_path):
        shutil.rmtree(folder_path)
    os.makedirs(folder_path)

    # Create test files with sample COBOL code
    test_files = [
        ("file1.txt", b"""
           IDENTIFICATION DIVISION.
           PROGRAM-ID. SAMPLE1.
           DATA DIVISION.
           WORKING-STORAGE SECTION.
           01 WS-NUMBER1 PIC 9(3) VALUE 123.
           PROCEDURE DIVISION.
           DISPLAY "Hello COBOL World!".
           STOP RUN.
        """),
        ("file2.txt", b"""
           IDENTIFICATION DIVISION.
           PROGRAM-ID. SAMPLE2.
           DATA DIVISION.
           WORKING-STORAGE SECTION.
           01 WS-NUMBER2 PIC 9(3) VALUE 456.
           PROCEDURE DIVISION.
           DISPLAY "Processing COBOL File 2".
           STOP RUN.
        """),
    ]

    # Prepare the files payload correctly
    files = [
        ("files", (name, content, "text/plain")) for name, content in test_files
    ]

    # Prepare the folder_path as a form field
    data = {
        "folder_path": folder_path
    }

    # Make POST request to the endpoint with streaming enabled
    url = "http://localhost:8000/process"
    response = requests.post(url, data=data, files=files, stream=True)

    # Check response status
    assert response.status_code == 200, f"Response failed: {response.text}"

    # Process the streamed response
    print("Processing streamed response:")
    for chunk in response.iter_content(chunk_size=1024):
        if chunk:  # Filter out keep-alive new chunks
            print(chunk.decode("utf-8"))

    # Clean up
    shutil.rmtree(folder_path)

if __name__ == "__main__":
    test_process_files_with_streaming()
