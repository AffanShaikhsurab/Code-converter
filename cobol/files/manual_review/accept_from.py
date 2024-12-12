import sys

def accept_from_file(filename: str) -> None:
    """Reads data from a file and prints it to the console.

    Args:
        filename: The path to the input file.
    """
    try:
        with open(filename, 'r') as file:
            for line in file:
                #Remove leading/trailing whitespace and print each line
                print(line.strip())
        return None #Return None even if successful
    except FileNotFoundError:
        print(f"Error: File '{filename}' not found.", file=sys.stderr)
        return None  #Explicitly return None on error
    except Exception as e:
        print(f"An error occurred: {e}", file=sys.stderr)
        return None #Explicitly return None on error

def main():
    """Main function to demonstrate file reading."""
    if len(sys.argv) != 2:
        print("Usage: python3 your_script_name.py <filename>", file=sys.stderr)
        sys.exit(1)

    filename = sys.argv[1]
    accept_from_file(filename)

if __name__ == "__main__":
    main()
