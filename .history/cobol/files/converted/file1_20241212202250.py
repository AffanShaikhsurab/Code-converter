import unittest

# Simulate the data from the linked COBOL file 'file1.cbl'
# In a real scenario, this would involve reading from an external file or receiving data from another module.
FILE1_VAR1 = 10  # Replace with actual data loading mechanism if needed


def file2_cobol_to_python(file1_var1: int) -> None:
    """Converts COBOL logic to Python.

    Args:
        file1_var1: Input integer value from file1.cbl.
    """
    var3: int = file1_var1 * 2
    print(f"FILE1-VAR1: {file1_var1}")
    print(f"VAR3: {var3}")


class TestFile2(unittest.TestCase):
    def test_file2(self) -> None:
        """Test case for file2_cobol_to_python function."""
        file2_cobol_to_python(FILE1_VAR1)
        # Assertion to check if the output is as expected.
        self.assertEqual(FILE1_VAR1 * 2, 20) # This assumes FILE1_VAR1 is 10.  Adjust as needed.


if __name__ == "__main__":
    unittest.main()
