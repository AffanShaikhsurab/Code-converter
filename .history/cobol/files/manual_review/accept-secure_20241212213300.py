import getpass


def get_secure_password() -> str:
    """Gets a password securely from the user.

    Returns:
        str: The password entered by the user.
    """
    password = getpass.getpass('Enter password: ')
    return password


def test_get_secure_password():
    """Tests the get_secure_password function. Note: This test doesn't actually verify the password's security,
    only that the function runs without error and returns a string. Robust password testing requires more advanced techniques."""
    password = get_secure_password()
    assert isinstance(password, str)

if __name__ == "__main__":
    test_get_secure_password()