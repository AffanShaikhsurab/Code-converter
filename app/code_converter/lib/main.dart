import 'package:code_converter/screens/github.dart';
import 'package:flutter/material.dart';
import 'package:http/http.dart' as http;
import 'dart:convert';
import 'package:url_launcher/url_launcher.dart';
import 'package:graphview/graphview.dart';
import 'package:flutter_animate/flutter_animate.dart';
import 'package:google_fonts/google_fonts.dart';

void main() {
  runApp(const MyApp());
}

class MyApp extends StatelessWidget {
  const MyApp({super.key});

  @override
  Widget build(BuildContext context) {
    return MaterialApp(
      title: 'COBOL to Python Converter',
      theme: ThemeData(
        colorScheme: ColorScheme.fromSeed(
          seedColor: const Color(0xFF6200EE),
          brightness: Brightness.light,
        ),
        useMaterial3: true,
        textTheme: GoogleFonts.interTextTheme(),
      ),
      home:  GithubAuthScreen(),
    );
}
}



class CallbackUrlDialog extends StatelessWidget {
  const CallbackUrlDialog({super.key});

  @override
  Widget build(BuildContext context) {
    final controller = TextEditingController();

    return AlertDialog(
      title: const Text('Enter Callback URL'),
      content: Column(
        mainAxisSize: MainAxisSize.min,
        crossAxisAlignment: CrossAxisAlignment.start,
        children: [
          TextField(
            controller: controller,
            decoration: const InputDecoration(
              hintText: 'Paste the callback URL here',
              prefixIcon: Icon(Icons.link),
              border: OutlineInputBorder(),
            ),
          ),
          const SizedBox(height: 8),
          Text(
            'Copy the URL from your browser after authorization',
            style: Theme.of(context).textTheme.bodySmall,
          ),
        ],
      ),
      actions: [
        TextButton(
          onPressed: () => Navigator.pop(context),
          child: const Text('Cancel'),
        ),
        FilledButton(
          onPressed: () => Navigator.pop(context, controller.text),
          child: const Text('Submit'),
        ),
      ],
    );
  }
}


// Continue with improved versions of CobolFolderScreen, ConversionScreen, 
// and DependencyGraphView following similar patterns of:
// - Error handling
// - Loading states
// - Modern UI components
// - Animations
// - Search functionality
// - Pull-to-refresh
// - Progress tracking
// - Detailed status updates

// I'll show the ConversionScreen as an example of the improvements:



enum StepStatus {
  pending,
  inProgress,
  completed,
  failed,
}

class ConversionStep {
  final String description;
  final StepStatus status;

  ConversionStep(this.description, this.status);
}

