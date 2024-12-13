from fastapi import FastAPI, HTTPException
from pydantic import BaseModel
import httpx
import os
from dotenv import load_dotenv

# Load environment variables from a .env file
load_dotenv()

app = FastAPI()

GITHUB_CLIENT_ID = os.getenv("GITHUB_CLIENT_ID")
GITHUB_CLIENT_SECRET = os.getenv("GITHUB_CLIENT_SECRET")
REDIRECT_URI = "http://localhost:8000/callback"

class TokenRequest(BaseModel):
    code: str

@app.post("/exchange-token")
async def exchange_token(request: TokenRequest):
    code = request.code

    # GitHub API URL for exchanging the code
    token_url = "https://github.com/login/oauth/access_token"

    # Prepare the request payload
    payload = {
        "client_id": GITHUB_CLIENT_ID,
        "client_secret": GITHUB_CLIENT_SECRET,
        "code": code,
        "redirect_uri": REDIRECT_URI,
    }

    headers = {"Accept": "application/json"}

    try:
        # Make a POST request to GitHub's token endpoint
        async with httpx.AsyncClient() as client:
            response = await client.post(token_url, data=payload, headers=headers)
            response_data = response.json()

        # Check if the response contains an access token
        if response.status_code == 200 and "access_token" in response_data:
            return response_data
        else:
            raise HTTPException(status_code=500, detail="Failed to exchange token: " + response_data.get("error", "Unknown error"))

    except Exception as e:
        raise HTTPException(status_code=500, detail=f"Failed to exchange token: {str(e)}")
