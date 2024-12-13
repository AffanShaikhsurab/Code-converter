import getpass
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
    return getpass.getpass(prompt)


def accept_from_source(source: str) -> str:
    """Accepts input from a specified source.

    Args:
        source: The source of the input ('env' for environment variables, 'cmd' for command-line arguments).

    Returns:
        The input from the specified source as a string.  Returns an error message if the source is invalid or input is not found.
    """
    if source == "env":
        try:
            return os.environ["MY_ENV_VAR"]
        except KeyError:
            return "Environment variable not found"
    elif source == "cmd":
        try:
            return sys.argv[1]
        except IndexError:
            return "No command-line argument provided"
    else:
        return "Invalid source specified"


# Test cases
def test_accept_input():
    with patch('builtins.input', return_value="Test Input"):
        assert accept_input("Enter input: ") == "Test Input"

def test_accept_secure_input():
    with patch('getpass.getpass', return_value="Secure Input"):
        assert accept_secure_input("Enter secure input: ") == "Secure Input"

def test_accept_from_source():
    os.environ["MY_ENV_VAR"] = "Env Input"
    assert accept_from_source("env") == "Env Input"
    with patch.object(sys, 'argv', ['script_name']): #Simulate no command line argument
        assert accept_from_source("cmd") == "No command-line argument provided"
    del os.environ["MY_ENV_VAR"]

if __name__ == "__main__":
    test_accept_input()
    test_accept_secure_input()
    test_accept_from_source()
    print("All test cases passed!")