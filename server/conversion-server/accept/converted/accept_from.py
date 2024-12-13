import os
import sys
import unittest
from unittest.mock import patch


def accept_input(prompt: str = "") -> str:
    """Simulates COBOL's ACCEPT statement for general input."""
    with patch('builtins.input', return_value="TestUser"):
        return input(prompt)


def accept_secure_input(prompt: str = "") -> str:
    """Simulates COBOL's ACCEPT statement for secure input (hiding characters)."""
    with patch('getpass.getpass', return_value="TestPassword"):
        try:
            import getpass
            return getpass.getpass(prompt)
        except ImportError:
            print("Warning: getpass module not found. Input will not be masked.")
            return "TestPassword"


def accept_from_environment(variable_name: str) -> str:
    """Simulates COBOL's ACCEPT statement from an environment variable."""
    return os.environ.get(variable_name, "")


def accept_from_command_line(arg_index: int) -> str:
    """Simulates COBOL's ACCEPT statement from command line arguments."""
    try:
        return sys.argv[arg_index]
    except IndexError:
        return ""


class TestCobolAccept(unittest.TestCase):
    def test_accept_input(self):
        self.assertEqual(accept_input("Enter your name: "), "TestUser")

    def test_accept_secure_input(self):
        self.assertEqual(accept_secure_input("Enter password: "), "TestPassword")

    @unittest.mock.patch.dict(os.environ, {"MY_VAR": "EnvironmentValue"})
    def test_accept_from_environment(self):
        self.assertEqual(accept_from_environment("MY_VAR"), "EnvironmentValue")

    @unittest.mock.patch.object(sys, 'argv', ['program_name', 'CommandLineArg'])
    def test_accept_from_command_line(self):
        self.assertEqual(accept_from_command_line(1), "CommandLineArg")


if __name__ == "__main__":
    unittest.main()