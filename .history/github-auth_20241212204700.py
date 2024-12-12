# GitHub OAuth Configuration
GITHUB_CLIENT_ID = 'Ov23liUyXsesP4nHuerk'  # Replace with your GitHub OAuth App Client ID
GITHUB_CLIENT_SECRET = '1a12db1d38254b29284f9752e57374aa6936e789'  # Replace with your GitHub OAuth App Client Secret
GITHUB_REDIRECT_URI = 'http://localhost:8000/callback'

class GitHubAuthorizer:
    def __init__(self):
        self.access_token = None

    def authorize(self):
        """
        Initiate GitHub OAuth authorization flow.
        Opens browser for user to authorize the application.
        """
        # Generate authorization URL
        auth_url = (
            f"https://github.com/login/oauth/authorize"
            f"?client_id={GITHUB_CLIENT_ID}"
            f"&redirect_uri={GITHUB_REDIRECT_URI}"
            f"&scope=repo"
        )
        
        print("Opening browser for GitHub authorization...")
        webbrowser.open(auth_url)

        # Prompt user to enter the code
        print("\nAfter authorizing, GitHub will redirect you to a page with a code.")
        print("Please copy the entire URL from the redirect page and paste it here:")
        redirect_url = input("Redirect URL: ")

        # Extract code from the redirect URL
        try:
            code = redirect_url.split('code=')[1].split('&')[0]
        except IndexError:
            print("Invalid redirect URL. Authorization failed.")
            return None

        # Exchange code for access token
        token_response = requests.post(
            'https://github.com/login/oauth/access_token',
            data={
                'client_id': GITHUB_CLIENT_ID,
                'client_secret': GITHUB_CLIENT_SECRET,
                'code': code,
                'redirect_uri': GITHUB_REDIRECT_URI
            },
            headers={'Accept': 'application/json'}
        )

        token_data = token_response.json()
        self.access_token = token_data.get('access_token')

        if not self.access_token:
            print("Failed to obtain access token.")
            return None

        return self.access_token

    def list_repositories(self):
        """
        List user's repositories after authorization.
        """
        if not self.access_token:
            print("Please authorize first.")
            return []

        g = Github(self.access_token)
        try:
            repos = list(g.get_user().get_repos())
            print("Your Repositories:")
            for i, repo in enumerate(repos, 1):
                print(f"{i}. {repo.full_name}")
            return repos
        except GithubException as e:
            print(f"Error fetching repositories: {e}")
            return []

    def select_repository(self):
        """
        Allow user to select a repository interactively.
        """
        repos = self.list_repositories()
        if not repos:
            return None

        while True:
            try:
                selection = int(input("Enter the number of the repository to convert: "))
                if 1 <= selection <= len(repos):
                    return repos[selection - 1]
            except ValueError:
                print("Please enter a valid number.")

    def list_cobol_folders_in_repo(self, repo: Repository):
        """
        List folders containing COBOL files in a repository.
        """
        cobol_folders = []
        
        def find_cobol_folders(contents, current_path=''):
            for content in contents:
                if content.type == 'dir':
                    try:
                        # Check if the directory contains COBOL files
                        dir_contents = repo.get_contents(content.path)
                        has_cobol_files = any(
                            file.name.lower().endswith(('.cbl', '.cobol')) 
                            for file in dir_contents if file.type == 'file'
                        )
                        
                        if has_cobol_files:
                            cobol_folders.append(content.path)
                        
                        # Recursively check subdirectories
                        find_cobol_folders(dir_contents, content.path)
                        
                    except Exception as e:
                        print(f"Could not access directory {content.path}: {e}")

        try:
            contents = repo.get_contents('')
            find_cobol_folders(contents)
            
            print("COBOL Folders Found:")
            for i, folder in enumerate(cobol_folders, 1):
                print(f"{i}. {folder}")
            
            return cobol_folders
        except GithubException as e:
            print(f"Error finding COBOL folders: {e}")
            return []

    def select_cobol_folder(self):
        """
        Allow user to select a folder containing COBOL files interactively.
        """
        cobol_folders = self.list_cobol_folders_in_repo(self.repo)
        if not cobol_folders:
            return None

        while True:
            try:
                selection = int(input("Enter the number of the folder to convert: "))
                if 1 <= selection <= len(cobol_folders):
                    return cobol_folders[selection - 1]
            except ValueError:
                print("Please enter a valid number.")

    def download_cobol_folder(self, repo: Repository, folder_path: str, output_folder: str):
        """
        Download all COBOL files from a specific folder in the repository.
        """
        os.makedirs(output_folder, exist_ok=True)
        
        try:
            folder_contents = repo.get_contents(folder_path)
            
            for content in folder_contents:
                if content.type == 'file' and content.name.lower().endswith(('.cbl', '.cobol')):
                    try:
                        local_file_path = os.path.join(output_folder, content.name)
                        
                        with open(local_file_path, 'wb') as f:
                            f.write(content.decoded_content)
                        
                        print(f"Downloaded: {content.path} -> {local_file_path}")
                    except Exception as e:
                        print(f"Error downloading {content.path}: {e}")
        except Exception as e:
            print(f"Error accessing folder {folder_path}: {e}")

def github_cobol_conversion_workflow():
    """
    Main workflow for GitHub COBOL to Python conversion.
    """
    # GitHub Authorization
    authorizer = GitHubAuthorizer()
    
    # Step 1: Authorize GitHub access
    print("Starting GitHub Authorization...")
    access_token = authorizer.authorize()
    if not access_token:
        print("Authorization failed. Exiting.")
        return

    # Step 2: Select Repository
    print("\nSelect a Repository to Convert COBOL Files...")
    repo = authorizer.select_repository()
    if not repo:
        print("No repository selected. Exiting.")
        return

    # Step 3: Find COBOL Folders
    print(f"\nSearching for folders with COBOL files in {repo.full_name}...")
    authorizer.repo = repo  # Add repo as an attribute for folder selection
    cobol_folders = authorizer.list_cobol_folders_in_repo(repo)
    if not cobol_folders:
        print("No COBOL folders found. Exiting.")
        return

    # Step 4: Select COBOL Folder
    print("\nSelect a COBOL folder to convert:")
    selected_folder = authorizer.select_cobol_folder()
    if not selected_folder:
        print("No folder selected. Exiting.")
        return

    # Step 5: Download Folder Contents
    local_cobol_folder = "./cobol/files"
    os.makedirs(local_cobol_folder, exist_ok=True)
    authorizer.download_cobol_folder(repo, selected_folder, local_cobol_folder)

    # Step 6: Run COBOL to Python Conversion
    print("\nStarting COBOL to Python Conversion...")
    process_files(local_cobol_folder)


def github_cobol_conversion_workflow():
    """
    Main workflow for GitHub COBOL to Python conversion.
    """
    # GitHub Authorization
    authorizer = GitHubAuthorizer()
    
    # Step 1: Authorize GitHub access
    print("Starting GitHub Authorization...")
    access_token = authorizer.authorize()
    if not access_token:
        print("Authorization failed. Exiting.")
        return

    # Step 2: Select Repository
    print("\nSelect a Repository to Convert COBOL Files...")
    repo = authorizer.select_repository()
    if not repo:
        print("No repository selected. Exiting.")
        return

    # Step 3: Find COBOL Files
    print(f"\nSearching for COBOL files in {repo.full_name}...")
    cobol_files = authorizer.list_cobol_folders_in_repo(repo)
    if not cobol_files:
        print("No COBOL files found. Exiting.")
        return

    # Step 4: Select COBOL Files to Convert
    print("\nSelect COBOL files to convert (enter numbers separated by space):")
    selected_indices = input("Enter file numbers: ").split()
    selected_files = [cobol_files[int(i)-1] for i in selected_indices if 1 <= int(i) <= len(cobol_files)]

    # Step 5: Download Files
    local_cobol_folder = "./cobol/files"
    authorizer.download_cobol_folder(repo, selected_files, local_cobol_folder)

    # Step 6: Run COBOL to Python Conversion
    print("\nStarting COBOL to Python Conversion...")
    
    
    
    
    
    
    