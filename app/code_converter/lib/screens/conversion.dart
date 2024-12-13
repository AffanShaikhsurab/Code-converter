import 'package:flutter/material.dart';
import 'package:flutter_animate/flutter_animate.dart';
import 'package:http/http.dart' as http;
import 'dart:convert';
import 'package:dio/dio.dart';
import 'dart:html' as html;
import 'dart:typed_data';

class ConversionScreen extends StatefulWidget {
  final String accessToken;
  final dynamic repository;
  final String folderPath;

  const ConversionScreen({
    super.key,
    required this.accessToken,
    required this.repository,
    required this.folderPath,
  });

  @override
  State<ConversionScreen> createState() => _ConversionScreenState();
}

class _ConversionScreenState extends State<ConversionScreen> {
  bool isProcessing = false;
  double downloadProgress = 0.0;
  double uploadProgress = 0.0;
  List<Map<String, dynamic>> convertedFiles = [];
  String? error;
  final dio = Dio();
  List<Uint8List> downloadedFiles = [];
  List<String> fileNames = [];
  
  @override
  void initState() {
    super.initState();
    startProcessing();
  }

  Future<void> startProcessing() async {
    setState(() {
      isProcessing = true;
      error = null;
      downloadedFiles.clear();
      fileNames.clear();
    });

    try {
      // Step 1: Download files from GitHub
      await downloadGithubFolder();
      setState(() => downloadProgress = 1.0);

      // Step 2: Upload files to conversion server
      final response = await uploadFiles();
      setState(() => uploadProgress = 1.0);

      // Step 3: Process conversion results
      if (response.statusCode == 200) {
        final result = json.decode(response.body);
        if (result['status'] == 'success') {
          setState(() {
            convertedFiles = List<Map<String, dynamic>>.from(result['converted_files']);
          });
        } else {
          throw Exception(result['message']);
        }
      } else {
        throw Exception('Server error: ${response.statusCode}');
      }
    } catch (e) {
      setState(() => error = e.toString());
    } finally {
      setState(() => isProcessing = false);
    }
  }

  Future<void> downloadGithubFolder() async {
    try {
      final contentsUrl = widget.repository['contents_url']
          .toString()
          .replaceAll('{+path}', widget.folderPath);
          
      final response = await http.get(
        Uri.parse(contentsUrl),
        headers: {
          'Authorization': 'Bearer ${widget.accessToken}',
          'Accept': 'application/json',
        },
      );

      if (response.statusCode == 200) {
        final contents = json.decode(response.body) as List;
        
        for (var i = 0; i < contents.length; i++) {
          final file = contents[i];
          if (file['type'] == 'file') {
            final downloadUrl = file['download_url'];
            final fileName = file['name'];
            
            final fileResponse = await dio.get<Uint8List>(
              downloadUrl,
              options: Options(responseType: ResponseType.bytes),
              onReceiveProgress: (received, total) {
                setState(() {
                  downloadProgress = (i + received / total) / contents.length;
                });
              },
            );
            
            if (fileResponse.data != null) {
              downloadedFiles.add(fileResponse.data!);
              fileNames.add(fileName);
            }
          }
        }
      }
    } catch (e) {
      throw Exception('Failed to download files: $e');
    }
  }

  Future<http.Response> uploadFiles() async {
    try {
      final url = Uri.parse('YOUR_SERVER_URL/process');
      var request = http.MultipartRequest('POST', url)
        ..fields['folder_path'] = widget.folderPath;

      for (var i = 0; i < downloadedFiles.length; i++) {
        final fileBytes = downloadedFiles[i];
        final fileName = fileNames[i];
        
        request.files.add(
          http.MultipartFile.fromBytes(
            'files',
            fileBytes,
            filename: fileName,
          ),
        );
        
        setState(() {
          uploadProgress = (i + 1) / downloadedFiles.length;
        });
      }

      final streamedResponse = await request.send();
      return await http.Response.fromStream(streamedResponse);
    } catch (e) {
      throw Exception('Failed to upload files: $e');
    }
  }

  void downloadConvertedFile(String fileName, String code) {
    // Create a Blob containing the file content
    final bytes = utf8.encode(code);
    final blob = html.Blob([bytes]);
    
    // Create a download URL
    final url = html.Url.createObjectUrlFromBlob(blob);
    
    // Create an anchor element and trigger download
    final anchor = html.AnchorElement(href: url)
      ..setAttribute("download", fileName.replaceAll('.cbl', '.py'))
      ..click();

    // Clean up
    html.Url.revokeObjectUrl(url);
    
    ScaffoldMessenger.of(context).showSnackBar(
      const SnackBar(content: Text('File download started')),
    );
  }

  @override
  Widget build(BuildContext context) {
    return Scaffold(
      appBar: AppBar(
        title: const Text('Converting COBOL to Python'),
      ),
      body: Column(
        children: [
          if (isProcessing) ...[
            LinearProgressIndicator(value: downloadProgress),
            const SizedBox(height: 8),
            Text('Downloading files: ${(downloadProgress * 100).toStringAsFixed(1)}%'),
            const SizedBox(height: 16),
            LinearProgressIndicator(value: uploadProgress),
            const SizedBox(height: 8),
            Text('Processing files: ${(uploadProgress * 100).toStringAsFixed(1)}%'),
          ],
          
          if (error != null)
            Card(
              margin: const EdgeInsets.all(16),
              color: Theme.of(context).colorScheme.errorContainer,
              child: Padding(
                padding: const EdgeInsets.all(16),
                child: Column(
                  children: [
                    Icon(
                      Icons.error_outline,
                      color: Theme.of(context).colorScheme.error,
                      size: 32,
                    ),
                    const SizedBox(height: 8),
                    Text(
                      'Conversion Error',
                      style: Theme.of(context).textTheme.titleMedium?.copyWith(
                        color: Theme.of(context).colorScheme.error,
                      ),
                    ),
                    const SizedBox(height: 8),
                    Text(error!),
                    const SizedBox(height: 16),
                    FilledButton.icon(
                      onPressed: startProcessing,
                      icon: const Icon(Icons.refresh),
                      label: const Text('Retry'),
                    ),
                  ],
                ),
              ),
            ),
            
          if (convertedFiles.isNotEmpty)
            Expanded(
              child: Card(
                margin: const EdgeInsets.all(16),
                child: Column(
                  crossAxisAlignment: CrossAxisAlignment.stretch,
                  children: [
                    Padding(
                      padding: const EdgeInsets.all(16),
                      child: Text(
                        'Converted Files',
                        style: Theme.of(context).textTheme.titleLarge,
                      ),
                    ),
                    Expanded(
                      child: ListView.builder(
                        itemCount: convertedFiles.length,
                        itemBuilder: (context, index) {
                          final file = convertedFiles[index];
                          return ListTile(
                            leading: const Icon(Icons.file_present),
                            title: Text(file['file_name']),
                            trailing: IconButton(
                              icon: const Icon(Icons.download),
                              onPressed: () => downloadConvertedFile(
                                file['file_name'],
                                file['converted_code'],
                              ),
                            ),
                          ).animate().fadeIn(
                            delay: Duration(milliseconds: index * 100),
                          );
                        },
                      ),
                    ),
                  ],
                ),
              ),
            ),
        ],
      ),
    );
  }
}