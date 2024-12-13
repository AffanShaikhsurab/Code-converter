
import 'package:code_converter/main.dart';
import 'package:code_converter/screens/github.dart';
import 'package:code_converter/screens/repo.dart';
import 'package:flutter/material.dart';
import 'package:http/http.dart' as http;
import 'dart:convert';
import 'package:url_launcher/url_launcher.dart';
import 'package:graphview/graphview.dart';
import 'package:flutter_animate/flutter_animate.dart';
import 'package:google_fonts/google_fonts.dart';
class GithubAuthScreen extends StatefulWidget {
  const GithubAuthScreen({super.key});

  @override
  State<GithubAuthScreen> createState() => _GithubAuthScreenState();
}

class _GithubAuthScreenState extends State<GithubAuthScreen> {
  bool isAuthorizing = false;
  String? error;

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
            final accessToken = await exchangeCodeForToken(code);
            if (mounted) {
              Navigator.pushReplacement(
                context,
                MaterialPageRoute(
                  builder: (context) => RepositoryListScreen(accessToken: accessToken),
                ),
              );
            }
          }
        }
      }
    } catch (e) {
      setState(() {
        print( 'Failed to authorize: ${e.toString()} ');

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
      body: Container(
        decoration: BoxDecoration(
          gradient: LinearGradient(
            begin: Alignment.topLeft,
            end: Alignment.bottomRight,
            colors: [
              Theme.of(context).colorScheme.primaryContainer,
              Theme.of(context).colorScheme.surface,
            ],
          ),
        ),
        child: Center(
          child: Card(
            elevation: 8,
            child: Padding(
              padding: const EdgeInsets.all(32.0),
              child: Column(
                mainAxisSize: MainAxisSize.min,
                children: [
                  const Icon(Icons.transform_rounded, size: 64)
                      .animate()
                      .fadeIn()
                      .scale(),
                  const SizedBox(height: 24),
                  Text(
                    'COBOL to Python Converter',
                    style: Theme.of(context).textTheme.headlineMedium,
                  ).animate().fadeIn().slide(),
                  const SizedBox(height: 16),
                  Text(
                    'Convert your COBOL codebase to modern Python',
                    style: Theme.of(context).textTheme.bodyLarge,
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
                  FilledButton.icon(
                    onPressed: isAuthorizing ? null : authorizeGithub,
                    icon: const Icon(Icons.lock_open),
                    label: Text(
                      isAuthorizing ? 'Authorizing...' : 'Authorize with GitHub',
                    ),
                  ),
                ],
              ),
            ),
          ),
        ),
      ),
    );
  }
}