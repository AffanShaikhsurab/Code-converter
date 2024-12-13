import asyncio
import httpx
import os

async def test_process_files():
    url = "http://localhost:8000/process"
    folder_path = "test_folder"
    test_files = [
        ("files", ("file1.txt", b"""
           IDENTIFICATION DIVISION.
           PROGRAM-ID. SAMPLE1.
           DATA DIVISION.
           WORKING-STORAGE SECTION.
           01 WS-NUMBER1 PIC 9(3) VALUE 123.
           PROCEDURE DIVISION.
           DISPLAY "Hello COBOL World!".
           STOP RUN.
        """, "text/plain")),
        ("files", ("file2.txt", b"""
           IDENTIFICATION DIVISION.
           PROGRAM-ID. SAMPLE2.
           DATA DIVISION.
           WORKING-STORAGE SECTION.
           01 WS-NUMBER2 PIC 9(3) VALUE 456.
           PROCEDURE DIVISION.
           DISPLAY "Processing COBOL File 2".
           STOP RUN.
        """, "text/plain")),
    ]

    # Ensure the test folder exists
    if not os.path.exists(folder_path):
        os.makedirs(folder_path)

    # Start the test
    async with httpx.AsyncClient(timeout=None) as client:
        # Make the request
        response = await client.post(
            url,
            files=test_files,
            data={"folder_path": folder_path},
            headers={"Accept": "text/event-stream"}
        )

        if response.status_code == 200:
            print("Connection established. Listening for events...\n")
            async for line in response.aiter_lines():
                if line.startswith("data: "):
                    # Process each line of SSE
                    event_data = line[6:]  # Remove 'data: ' prefix
                    print(f"Received Event: {event_data}")
        else:
            print(f"Error: {response.status_code}, {response.text}")


if __name__ == "__main__":
    asyncio.run(test_process_files())
