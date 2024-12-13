import os
import sys
from unittest.mock import patch


def accept_input(prompt: str = "") -> str:
    """Accepts user input from the console."""
    return input(prompt)


def accept_secure_input(prompt: str = "") -> str:
    """Accepts secure user input (hidden from display)."""
    return getpass.getpass(prompt)


def accept_from_environment(variable_name: str) -> str:
    """Accepts input from an environment variable."""
    return os.getenv(variable_name, "")


def accept_from_command_line(index: int) -> str:
    """Accepts input from command-line arguments."""
    try:
        return sys.argv[index]
    except IndexError:
        return ""


# Example Usage and Test Cases
def test_accept_functions():
    # Mock inputs for testing
    mock_console_input = "Console Input"
    mock_secure_input = "Secure Input"
    mock_environment_variable = "Environment Input"
    mock_command_line_arg = "Command Line Input"

    # Simulate console input
    with patch('builtins.input', return_value=mock_console_input):
        console_input = accept_input("")
        assert console_input == mock_console_input

    # Simulate secure input
    with patch('getpass.getpass', return_value=mock_secure_input):
        secure_input = accept_secure_input("")
        assert secure_input == mock_secure_input

    # Simulate environment variable
    with patch.dict(os.environ, {'TEST_VARIABLE': mock_environment_variable}, clear=True):
        environment_input = accept_from_environment('TEST_VARIABLE')
        assert environment_input == mock_environment_variable

    # Simulate command-line argument
    with patch('sys.argv', ['', mock_command_line_arg]):
        command_line_input = accept_from_command_line(1)
        assert command_line_input == mock_command_line_arg


if __name__ == "__main__":
    test_accept_functions()