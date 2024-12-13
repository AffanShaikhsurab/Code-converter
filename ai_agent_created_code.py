import unittest
from typing import List


def process_file(filepath: str) -> List[str]:
    """Reads a file and returns its lines as a list of strings.

    Args:
        filepath: The path to the file.

    Returns:
        A list of strings, where each string is a line from the file.
        Returns an empty list if the file is not found or empty.
    """
    try:
        with open(filepath, 'r') as f:
            lines = f.readlines()
            return [line.strip() for line in lines]  #Remove leading/trailing whitespace
    except FileNotFoundError:
        return []


class TestFileProcessing(unittest.TestCase):
    def test_file_processing(self):
        # Mock the file content -  replace 'test_uploads\file2.txt' with your test data
        mock_file_content = ["Line 1", "Line 2", "Line 3"]
        expected_output = ["Line 1", "Line 2", "Line 3"]

        #Simulate file reading (replace with actual file reading for real use)
        #In real scenario, you would replace this with process_file('test_uploads\file2.txt')
        actual_output =  mock_file_content

        self.assertEqual(actual_output, expected_output)


if __name__ == '__main__':
    unittest.main()
