import os
import sys
from unittest.mock import patch


def accept_input(prompt: str = "") -> str:
    """Accepts input from the user.

    Args:
        prompt: The prompt to display to the user (optional).

    Returns:
        The user's input as a string.
    """
    return input(prompt)

def accept_secure_input(prompt: str = "") -> str:
    """Accepts secure input from the user (input is not displayed).

    Args:
        prompt: The prompt to display to the user (optional).

    Returns:
        The user's input as a string.
    """
    import getpass  # Use getpass for secure input
    try:
        return getpass.getpass(prompt)
    except Exception as e:
        return f"Error: {e}"

def accept_from_environment(variable_name: str) -> str:
    """Accepts input from an environment variable.

    Args:
        variable_name: The name of the environment variable.

    Returns:
        The value of the environment variable as a string, or None if not found.
    """
    return os.environ.get(variable_name)

def accept_from_command_line(index: int) -> str:
    """Accepts input from the command line arguments.

    Args:
        index: The index of the command-line argument (starting from 1).

    Returns:
        The command-line argument as a string, or None if not found.
    """
    try:
        return sys.argv[index]
    except IndexError:
        return None


def test_accept_input():
    with patch('builtins.input', return_value="Test User"):
        assert accept_input("Enter your name: ") == "Test User"

def test_accept_secure_input():
    with patch('getpass.getpass', return_value="TestPassword"):
        assert accept_secure_input("Enter your password: ") == "TestPassword"

def test_accept_from_environment():
    with patch.dict(os.environ, {"TEST_VAR": "TestEnvironment"}, clear=True):
        assert accept_from_environment("TEST_VAR") == "TestEnvironment"

def test_accept_from_command_line():
    with patch.object(sys, 'argv', ['program_name', 'command_line_arg1']):
        assert accept_from_command_line(1) == "command_line_arg1"

if __name__ == "__main__":
    test_accept_input()
    test_accept_secure_input()
    test_accept_from_environment()
    test_accept_from_command_line()
    print("All test cases passed!")