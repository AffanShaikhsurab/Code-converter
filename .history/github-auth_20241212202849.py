import os
import sys
import subprocess
import webbrowser
import networkx as nx
from typing import Dict, List, Optional

# Additional imports for GitHub integration
import requests
from github import Github, GithubException
from github.Repository import Repository
from github.ContentFile import ContentFile

# Keep existing imports from the original script
from asyncio.log import logger
from fastapi import HTTPException
from groq import Groq
from pydantic import BaseModel, Field
from langchain_google_genai import ChatGoogleGenerativeAI
from langchain.prompts import PromptTemplate
from langchain.output_parsers import PydanticOutputParser

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

    def list_cobol_files_in_repo(self, repo: Repository):
        """
        List COBOL files in a repository.
        """
        cobol_files = []
        
        def find_cobol_files(contents, path=''):
            for content in contents:
                if content.type == 'file' and content.name.lower().endswith(('.cbl', '.cobol')):
                    cobol_files.append(os.path.join(path, content.name))
                elif content.type == 'dir':
                    try:
                        dir_contents = repo.get_contents(content.path)
                        find_cobol_files(dir_contents, content.path)
                    except Exception as e:
                        print(f"Could not access directory {content.path}: {e}")

        try:
            contents = repo.get_contents('')
            find_cobol_files(contents)
            
            print("COBOL Files Found:")
            for i, file in enumerate(cobol_files, 1):
                print(f"{i}. {file}")
            
            return cobol_files
        except GithubException as e:
            print(f"Error finding COBOL files: {e}")
            return []

    def download_cobol_files(self, repo: Repository, files: List[str], output_folder: str):
        """
        Download selected COBOL files from the repository.
        """
        os.makedirs(output_folder, exist_ok=True)
        
        for file_path in files:
            try:
                file_content = repo.get_contents(file_path)
                local_file_path = os.path.join(output_folder, os.path.basename(file_path))
                
                with open(local_file_path, 'wb') as f:
                    f.write(file_content.decoded_content)
                
                print(f"Downloaded: {file_path} -> {local_file_path}")
            except Exception as e:
                print(f"Error downloading {file_path}: {e}")

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
    cobol_files = authorizer.list_cobol_files_in_repo(repo)
    if not cobol_files:
        print("No COBOL files found. Exiting.")
        return

    # Step 4: Select COBOL Files to Convert
    print("\nSelect COBOL files to convert (enter numbers separated by space):")
    selected_indices = input("Enter file numbers: ").split()
    selected_files = [cobol_files[int(i)-1] for i in selected_indices if 1 <= int(i) <= len(cobol_files)]

    # Step 5: Download Files
    local_cobol_folder = "./cobol/files"
    authorizer.download_cobol_files(repo, selected_files, local_cobol_folder)

    # Step 6: Run COBOL to Python Conversion
    print("\nStarting COBOL to Python Conversion...")
    process_files(local_cobol_folder)