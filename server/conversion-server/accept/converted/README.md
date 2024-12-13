import os


def accept_readme(filename: str = "README.md") -> str:
    """Simulates the COBOL ACCEPT statement to read the content of a file.

    Args:
        filename: The name of the file to read. Defaults to "README.md".

    Returns:
        The content of the file, or an error message if the file is not found.
    """
    try:
        with open(filename, "r") as f:
            content = f.read()
            return f"Content of {filename}:\n{content}"
    except FileNotFoundError:
        return f"Error: File '{filename}' not found."


def test_accept_readme():
    mock_readme_content = "This is a mock README file.\n"
    temp_filename = "test_README.md"
    with open(temp_filename, "w") as f:
        f.write(mock_readme_content)

    actual_output = accept_readme(temp_filename)
    expected_output = f"Content of {temp_filename}:\n{mock_readme_content}"
    assert actual_output == expected_output

    os.remove(temp_filename)


test_accept_readme()