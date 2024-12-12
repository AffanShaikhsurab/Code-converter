import sys

def accept_input(prompt: str) -> str:
    """Accepts user input with a specified prompt.

    Args:
        prompt: The prompt to display to the user.

    Returns:
        The user's input as a string.
    """
    try:
        user_input = input(prompt)
        return user_input
    except (EOFError, KeyboardInterrupt, OSError) as e:
        print(f"Input cancelled: {e}", file=sys.stderr)
        return ""

def main():
    """Main function to demonstrate the accept_input function."""
    name = accept_input("Enter your name: ")
    print(f"Hello, {name}!")

if __name__ == "__main__":
    main()
