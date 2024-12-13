import 'package:code_converter/auth.dart';
import 'package:code_converter/main.dart';
import 'package:code_converter/screens/repo.dart';
import 'package:flutter/material.dart';
import 'package:flutter_animate/flutter_animate.dart';
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
    _checkExistingAuth();
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
        builder: (context) => RepositoryListScreen(accessToken: token),
      ),
    );
  }
  Future<void> authorizeGithub() async {
    setState(() {
      isAuthorizing = true;
      error = null;
    });

    try {
      final Uri authUrl = Uri.parse(
        'https://github.com/login/oauth/authorize'
        '?client_id=Ov23liUyXsesP4nHuerk'
        '&redirect_uri=http://localhost:8000/callback'
        '&scope=repo',
      );

      if (await canLaunchUrl(authUrl)) {
        await launchUrl(authUrl);
        final String? callbackUrl = await showDialog(
          context: context,
          barrierDismissible: false,
          builder: (context) => const CallbackUrlDialog(),
        );

        if (callbackUrl != null) {
          final code = Uri.parse(callbackUrl).queryParameters['code'];
          if (code != null) {
            accessToken = await exchangeCodeForToken(code);
              // Save the token using our new AuthService
            await AuthService.saveToken(accessToken!);
            
           
            if (mounted) {
              Navigator.pushReplacement(
                context,
                MaterialPageRoute(
                  builder: (context) =>  RepositoryListScreen(accessToken: accessToken!,),
                ),
              );
            }
          }
        }
      }
    } catch (e) {
      setState(() {
        print('Failed to authorize: ${e.toString()}');
        error = 'Failed to authorize: ${e.toString()}';
      });
    } finally {
      setState(() {
        isAuthorizing = false;
      });
    }
  }

  Future<String> exchangeCodeForToken(String code) async {
    final response = await http.post(
      Uri.parse('https://github.com/login/oauth/access_token'),
      headers: {'Accept': 'application/json'},
      body: {
        'client_id': 'Ov23liUyXsesP4nHuerk',
        'client_secret': '1a12db1d38254b29284f9752e57374aa6936e789',
        'code': code,
        'redirect_uri': 'http://localhost:8000/callback',
      },
    );

    if (response.statusCode != 200) {
      throw Exception('Failed to exchange code for token');
    }

    final tokenData = json.decode(response.body);
    return tokenData['access_token'];
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
