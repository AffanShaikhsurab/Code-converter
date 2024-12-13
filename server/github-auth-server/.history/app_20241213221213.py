from fastapi import FastAPI, HTTPException
from fastapi.responses import JSONResponse
from pydantic import BaseModel
from fastapi.middleware.cors import CORSMiddleware  # Import CORSMiddleware
import httpx
import os

app = FastAPI()

# GitHub OAuth credentials
CLIENT_ID = 'Ov23liUyXsesP4nHuerk'
CLIENT_SECRET = '0c188d5b0062fe8420f46a6b24b34b8800e5d53b'
REDIRECT_URI = 'https://legacy-code-converter.web.app/callback'

# CORS configuration
origins = [
       "http://localhost:63859", 
    "http://localhost:3000",  # Your local frontend URL (if running locally for testing)
    "https://legacy-code-converter.web.app",  # Your deployed frontend URL
]

app.add_middleware(
    CORSMiddleware,
    allow_origins=origins,  # Allow these origins
    allow_credentials=True,
    allow_methods=["*"],  # Allow all HTTP methods (GET, POST, etc.)
    allow_headers=["*"],  # Allow all headers
)

class CodeRequest(BaseModel):
    code: str
@app.post("/exchange_code")
async def exchange_code(request: CodeRequest):
    try:
        url = 'https://github.com/login/oauth/access_token'
        data = {
            'client_id': CLIENT_ID,
            'client_secret': CLIENT_SECRET,
            'code': request.code,
            'redirect_uri': REDIRECT_URI,
        }
        
        async with httpx.AsyncClient() as client:
            response = await client.post(url, data=data, headers={'Accept': 'application/json'})

        print(response.status_code)  # Log the status code
        print(response.text)  # Log the body of the response

        if response.status_code != 200:
            raise HTTPException(status_code=500, detail=f"Failed to exchange code for token: {response.text}")

        token_data = response.json()
        access_token = token_data.get('access_token')

        if not access_token:
            raise HTTPException(status_code=400, detail="Access token not found in response")

        return JSONResponse(content={"access_token": access_token})

    except httpx.HTTPStatusError as e:
        raise HTTPException(status_code=500, detail=f"HTTP error: {str(e)}")
    except Exception as e:
        raise HTTPException(status_code=500, detail=f"An error occurred: {str(e)}")


if __name__ == "__main__":
    import uvicorn
    uvicorn.run(app, host="0.0.0.0", port=8000)
