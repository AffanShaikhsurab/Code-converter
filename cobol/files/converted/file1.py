import unittest


# Simulate the data from file1.cbl.  In a real scenario, this would involve
# reading from an external file or receiving data from another module.
FILE1_VAR1 = 10  # Replace with actual data reading if file1.cbl exists


def file2_logic(file1_var1: int) -> int:
    """Simulates the COBOL FILE2 program.

    Args:
        file1_var1: An integer value received from file1.cbl.

    Returns:
        The double of the input value.
    """
    print(f"Value from FILE1-VAR1: {file1_var1}")
    var3: int = file1_var1 * 2
    print(f"Doubled value (VAR3): {var3}")
    return var3


class TestFile2(unittest.TestCase):
    def test_file2_logic(self) -> None:
        """Test case for the file2_logic function."""
        self.assertEqual(file2_logic(FILE1_VAR1), 20)  # Assertion for FILE1_VAR1 = 10


if __name__ == "__main__":
    unittest.main()
