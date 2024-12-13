import sys
import os


def accept_input(prompt: str = "") -> str:
    """Accepts input from the user.

    Args:
        prompt: The prompt to display to the user (optional).

    Returns:
        The user's input.
    """
    return input(prompt)


def accept_secure_input(prompt: str = "") -> str:
    """Accepts secure input from the user (input is not displayed).

    Args:
        prompt: The prompt to display to the user (optional).

    Returns:
        The user's input.
    """
    import getpass  #Use getpass for secure input
    return getpass.getpass(prompt)


def accept_from_environment(variable_name: str) -> str:
    """Accepts input from an environment variable.

    Args:
        variable_name: The name of the environment variable.

    Returns:
        The value of the environment variable, or an empty string if it's not set.
    """
    return os.environ.get(variable_name, "")


def accept_from_command_line(index: int) -> str:
    """Accepts input from the command line arguments.

    Args:
        index: The index of the command line argument.

    Returns:
        The command line argument at the given index, or an empty string if it's out of bounds.
    """
    try:
        return sys.argv[index]
    except IndexError:
        return ""


#Test cases
def test_accept_input():
    mock_input = "Test Input"
    # Mock the input function
    input_mock = lambda x: mock_input
    # Use unittest.mock.patch to make the change temporary
    with unittest.mock.patch('builtins.input', input_mock):
        assert accept_input("Enter input:") == mock_input

def test_accept_secure_input():
    mock_input = "Secure Input"
    with unittest.mock.patch('getpass.getpass', return_value=mock_input):
        assert accept_secure_input("Enter secure input:") == mock_input

def test_accept_from_environment():
    #Set environment variable for testing
    os.environ["TEST_VAR"] = "Environment Input"
    assert accept_from_environment("TEST_VAR") == "Environment Input"
    del os.environ["TEST_VAR"] # Clean up after test
    assert accept_from_environment("NON_EXISTENT_VAR") == ""

def test_accept_from_command_line():
    #Mock sys.argv
    with unittest.mock.patch.object(sys, 'argv', ['program_name', 'Command', 'Line', 'Argument']):
        assert accept_from_command_line(1) == "Command"
        assert accept_from_command_line(3) == "Argument"
        assert accept_from_command_line(4) == ""

import unittest
if __name__ == "__main__":
    unittest.main()
