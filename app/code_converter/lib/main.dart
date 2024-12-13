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
          brightness: Brightness.dark,
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
      title: const Text('Enter Callback URL' , style: TextStyle(fontWeight: FontWeight.bold , color: Colors.white),),
      content: Column(
        mainAxisSize: MainAxisSize.min,
        crossAxisAlignment: CrossAxisAlignment.start,
        children: [
          TextField(
            controller: controller,
            decoration: const InputDecoration(
              hintText: 'Paste the callback URL here',
              hintStyle: TextStyle(color: Colors.white),
              prefixIcon: Icon(Icons.link),
              labelStyle: TextStyle(color: Colors.white),
              border: OutlineInputBorder(),
            ),
          ),
          const SizedBox(height: 8),
          Text(
            'Copy the URL from your browser after authorization',
            style: TextStyle(fontWeight: FontWeight.normal , color: const Color.fromARGB(255, 99, 99, 99)),
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

