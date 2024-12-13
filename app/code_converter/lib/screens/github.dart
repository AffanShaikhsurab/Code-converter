import 'package:code_converter/auth.dart';
import 'package:code_converter/main.dart';
import 'package:code_converter/screens/home.dart';
import 'package:code_converter/screens/repo.dart';
import 'package:flutter/material.dart';
import 'package:flutter_animate/flutter_animate.dart';
import 'package:flutter_web_auth/flutter_web_auth.dart';
import 'package:http/http.dart' as http;
import 'dart:convert';
import 'package:url_launcher/url_launcher.dart';
import 'package:google_fonts/google_fonts.dart';

class GithubAuthScreen extends StatefulWidget {
  const GithubAuthScreen({super.key});

  @override
  State<GithubAuthScreen> createState() => _GithubAuthScreenState();
}

class _GithubAuthScreenState extends State<GithubAuthScreen> {
  bool isAuthorizing = false;
  String? error;
  String? accessToken;
 @override
  void initState() {
    super.initState();
    _checkForCallbackUrl();
    _checkExistingAuth();
  }

  // Check if the URL contains '/callback' and extract the code
  Future<void> _checkForCallbackUrl() async {
    print("url is ${Uri.base}");
    print("callback is ${Uri.base.path.contains('/callback')}");
    print("query is ${Uri.base.queryParameters['code']}");
    final Uri currentUri = Uri.base;  // Get the current URL of the web page
    if (currentUri.path.contains('/callback')) {
      final String? code = currentUri.queryParameters['code'];
      if (code != null) {
        accessToken = await exchangeCodeForToken(code);
        showCongratulationsDialog(
          context
        );
        // Save the token using your new AuthService (if you have one)
        await AuthService.saveToken(accessToken!);

        if (mounted) {
          Navigator.pushReplacement(
            context,
            MaterialPageRoute(
              builder: (context) => RepositoryListScreen(accessToken: accessToken!,),
            ),
          );
        }
    
        setState(() {});
      }
    }
  }
  void showCongratulationsDialog(BuildContext context) {
  showDialog(
    context: context,
    builder: (BuildContext context) {
      return AlertDialog(
        title: Text('🎉 Congratulations!'),
        content: Text('You have successfully authenticated and logged in! Your access token has been securely retrieved and you are now ready to explore your repositories.'),
        actions: <Widget>[
          TextButton(
            child: Text('Let\'s Go!'),
            onPressed: () {
              Navigator.of(context).pop();
            },
          ),
        ],
      );
    },
  );
}
  Future<void> _checkExistingAuth() async {
    final token = await AuthService.getToken();
    if (token != null) {
      _navigateToRepositoryScreen(token);
    }
  }

  void _navigateToRepositoryScreen(String token) {
    Navigator.pushReplacement(
      context,
      MaterialPageRoute(
        builder: (context) => DashboardScreen(accessToken: token),
      ),
    );
  }
 
  // Use the hosted URL for redirect_uri
  final String redirectUri = 'https://legacy-code-converter.web.app/callback';

  Future<void> authorizeGithub() async {
    setState(() {
      isAuthorizing = true;
      error = null;
    });

    try {
      // GitHub OAuth URL with client_id and redirect URI
      final Uri authUrl = Uri.parse(
        'https://github.com/login/oauth/authorize'
        '?client_id=Ov23liUyXsesP4nHuerk'
        '&redirect_uri=$redirectUri'
        '&scope=repo',
      );

      // Use flutter_web_auth to authenticate the user
      final result = await FlutterWebAuth.authenticate(
        url: authUrl.toString(),
        callbackUrlScheme: "https", // The scheme should match the redirect_uri
      );

      // Extract the authorization code from the result URL
      final String? code = Uri.parse(result).queryParameters['code'];
      if (code != null) {
        // Use the code to exchange for an access token
        accessToken = await exchangeCodeForToken(code);
        
        // Save the token using your new AuthService (if you have one)
        await AuthService.saveToken(accessToken!);

        if (mounted) {
          Navigator.pushReplacement(
            context,
            MaterialPageRoute(
              builder: (context) => RepositoryListScreen(accessToken: accessToken!,),
            ),
          );
        }
      }
    } catch (e) {
      setState(() {
        error = 'Failed to authorize: ${e.toString()}';
      });
    } finally {
      setState(() {
        isAuthorizing = false;
      });
    }
  }
Future<String> exchangeCodeForToken(String code) async {
  try {
    // Replace this URL with your FastAPI backend URL
    final response = await http.post(
      Uri.parse('https://github-auth-server.onrender.com/exchange_code'), // Update with your backend URL
      headers: {'Content-Type': 'application/json'},
      body: json.encode({'code': code}), // Sending code to the backend
    );

    if (response.statusCode == 200) {
      // Parse the access token from the response
      final tokenData = json.decode(response.body);
      final accessToken = tokenData['access_token'];

      if (accessToken != null) {
        return accessToken;
      } else {
        throw Exception('Failed to retrieve access token');
      }
    } else {
      throw Exception('Failed to exchange code for token. Status: ${response.statusCode}');
    }
  } catch (e) {
    throw Exception('Error exchanging code for token: ${e.toString()}');
  }
}

  @override
  Widget build(BuildContext context) {
    return Scaffold(
      backgroundColor: const Color(0xFF1E1E1E),
      appBar: AppBar(
        title: Text(
          'COBOL to Python Converter',
          style: GoogleFonts.montserrat(
            fontWeight: FontWeight.bold,
            fontSize: 24,
          ),
        ),
        backgroundColor: const Color(0xFF1E1E1E),
        elevation: 0,
      ),
      drawer: const AppDrawer(),
      body: Center(
        child: Card(
          color: const Color(0xFF2F2F2F),
          elevation: 8,
          child: Padding(
            padding: const EdgeInsets.all(32.0),
            child: Column(
              mainAxisSize: MainAxisSize.min,
              children: [
                const Icon(Icons.transform_rounded, size: 64, color: Colors.white)
                    .animate()
                    .fadeIn()
                    .scale(),
                const SizedBox(height: 24),
                Text(
                  'Authorize with GitHub',
                  style: GoogleFonts.montserrat(
                    fontWeight: FontWeight.bold,
                    fontSize: 24,
                    color: Colors.white,
                  ),
                ).animate().fadeIn().slide(),
                const SizedBox(height: 16),
                Text(
                  'Convert your COBOL codebase to modern Python',
                  style: GoogleFonts.nunito(
                    fontSize: 18,
                    color: Colors.white.withOpacity(0.8),
                  ),
                ),
                const SizedBox(height: 32),
                if (error != null)
                  Padding(
                    padding: const EdgeInsets.only(bottom: 16),
                    child: Text(
                      error!,
                      style: TextStyle(
                        color: Theme.of(context).colorScheme.error,
                      ),
                    ),
                  ),
                ElevatedButton.icon(
                  onPressed: isAuthorizing ? null : authorizeGithub,
                  style: ElevatedButton.styleFrom(
                    backgroundColor: const Color(0xFF3366FF),
                    padding: const EdgeInsets.symmetric(horizontal: 24, vertical: 16),
                    shape: RoundedRectangleBorder(
                      borderRadius: BorderRadius.circular(12),
                    ),
                  ),
                  icon: const Icon(Icons.lock_open, color: Colors.white),
                  label: Text(
                    isAuthorizing ? 'Authorizing...' : 'Authorize with GitHub',
                    style: GoogleFonts.nunito(
                      fontWeight: FontWeight.bold,
                      fontSize: 18,
                      color: Colors.white,
                    ),
                  ),
                ),
              ],
            ),
          ),
        ),
      ),
    );
  }
}

class AppDrawer extends StatelessWidget {
  const AppDrawer({super.key});

  @override
  Widget build(BuildContext context) {
    return Drawer(
      backgroundColor: const Color(0xFF1E1E1E),
      child: ListView(
        children: [
          DrawerHeader(
            child: Text(
              'COBOL to Python Converter',
              style: GoogleFonts.montserrat(
                fontWeight: FontWeight.bold,
                fontSize: 24,
                color: Colors.white,
              ),
            ),
          ),
          ListTile(
            leading: const Icon(Icons.home, color: Colors.white),
            title: Text(
              'Home',
              style: GoogleFonts.nunito(
                fontSize: 18,
                color: Colors.white,
              ),
            ),
            onTap: () => Navigator.pushNamed(context, '/'),
          ),
          ListTile(
            leading: const Icon(Icons.transform_rounded, color: Colors.white),
            title: Text(
              'Converter',
              style: GoogleFonts.nunito(
                fontSize: 18,
                color: Colors.white,
              ),
            ),
            onTap: () => Navigator.pushNamed(context, '/converter'),
          ),
          ListTile(
            leading: const Icon(Icons.settings, color: Colors.white),
            title: Text(
              'Settings',
              style: GoogleFonts.nunito(
                fontSize: 18,
                color: Colors.white,
              ),
            ),
            onTap: () => Navigator.pushNamed(context, '/settings'),
          ),
        ],
      ),
    );
  }
}
