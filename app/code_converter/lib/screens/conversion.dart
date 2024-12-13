import 'package:flutter/material.dart';
import 'package:http/http.dart' as http;
import 'package:google_fonts/google_fonts.dart';
import 'dart:convert';
import 'dart:html' as html;

class ConversionScreen extends StatefulWidget {
  final String accessToken;
  final dynamic repository;
  final String folderPath;

  const ConversionScreen({
    Key? key,
    required this.accessToken,
    required this.repository,
    required this.folderPath,
  }) : super(key: key);

  @override
  State<ConversionScreen> createState() => _ConversionScreenState();
}

class _ConversionScreenState extends State<ConversionScreen>
    with SingleTickerProviderStateMixin {
  late TabController _tabController;
  bool isProcessing = false;
  List<String> terminalLogs = [];
  List<FileData> originalFiles = [];
  List<FileData> convertedFiles = [];
  String? error;
  double progress = 0.0;

  // Metrics
  double temperature = 0.6;
  int maxLength = 2200;
  double topP = 0.4;

  @override
  void initState() {
    super.initState();
    _tabController = TabController(length: 2, vsync: this);
    startProcessing();
  }

  @override
  void dispose() {
    _tabController.dispose();
    super.dispose();
  }

  void addLog(String message) {
    setState(() {
      terminalLogs.add(message);
    });
  }






  Future<void> startProcessing() async {
    setState(() {
      isProcessing = true;
      error = null;
      originalFiles.clear();
      convertedFiles.clear();
      terminalLogs.clear();
    });

    try {
      addLog('> Initializing conversion environment');
      await Future.delayed(const Duration(seconds: 1));

      addLog('> Loading repository: ${widget.folderPath}');
      await fetchGithubFiles();

      if (originalFiles.isNotEmpty) {
        addLog('> Processing files for conversion');
        await processFiles();
      }
    } catch (e) {
      setState(() => error = e.toString());
      addLog('> Error: $e');
    } finally {
      setState(() => isProcessing = false);
    }
  }

  Future<void> fetchGithubFiles() async {
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
        
        for (var file in contents) {
          if (file['type'] == 'file') {
            final fileContent = await fetchFileContent(file['download_url']);
            setState(() {
              originalFiles.add(FileData(
                name: file['name'],
                content: fileContent,
                size: file['size'],
                type: 'COBOL',
              ));
            });
          }
        }
        addLog('> Found ${originalFiles.length} files to convert');
      }
    } catch (e) {
      throw Exception('Failed to fetch files: $e');
    }
  }

  Future<String> fetchFileContent(String url) async {
    try {
      final response = await http.get(Uri.parse(url));
      return response.body;
    } catch (e) {
      throw Exception('Failed to fetch file content: $e');
    }
  }

  Future<void> processFiles() async {
    try {
      final url = Uri.parse('https://code-converter-pte2.onrender.com/process');
      var request = http.MultipartRequest('POST', url)
        ..fields['folder_path'] = widget.folderPath;

      for (var file in originalFiles) {
        request.files.add(
          http.MultipartFile.fromString(
            'files',
            file.content,
            filename: file.name,
          ),
        );
      }

      addLog('> Uploading files for conversion');
      final streamedResponse = await request.send();
      final response = await http.Response.fromStream(streamedResponse);

      if (response.statusCode == 200) {
        final result = json.decode(response.body);
        if (result['status'] == 'success') {
          setState(() {
            convertedFiles = (result['converted_files'] as List)
                .map((file) => FileData(
                      name: file['file_name'].toString().replaceAll('.cbl', '.py'),
                      content: file['converted_code'],
                      type: 'Python',
                      size: utf8.encode(file['converted_code']).length,
                    ))
                .toList();
          });
          
          addLog('> Conversion completed successfully');
          _tabController.animateTo(1); // Switch to converted files tab
        } else {
          throw Exception(result['message']);
        }
      } else {
        throw Exception('Server error: ${response.statusCode}');
      }
    } catch (e) {
      throw Exception('Failed to process files: $e');
    }
  }

  void downloadFile(FileData file) {
    final bytes = utf8.encode(file.content);
    final blob = html.Blob([bytes]);
    final url = html.Url.createObjectUrlFromBlob(blob);

    final anchor = html.AnchorElement(href: url)
      ..setAttribute("download", file.name)
      ..click();

    html.Url.revokeObjectUrl(url);

    ScaffoldMessenger.of(context).showSnackBar(
      SnackBar(content: Text('Downloading ${file.name}')),
    );
  }

  Widget _buildFileList(List<FileData> files, String title) {
    return Column(
      crossAxisAlignment: CrossAxisAlignment.stretch,
      children: [
        Padding(
          padding: const EdgeInsets.all(16),
          child: Text(
            title,
            style: GoogleFonts.inter(
              color: Colors.grey[300],
              fontSize: 16,
              fontWeight: FontWeight.w600,
            ),
          ),
        ),
        Expanded(
          child: files.isEmpty
              ? Center(
                  child: Text(
                    'No files available',
                    style: GoogleFonts.inter(color: Colors.grey[500]),
                  ),
                )
              : ListView.builder(
                  itemCount: files.length,
                  itemBuilder: (context, index) {
                    final file = files[index];
                    return ListTile(
                      dense: true,
                      leading: Icon(
                        Icons.code,
                        size: 20,
                        color: file.type == 'COBOL' ? Colors.orange : Colors.blue,
                      ),
                      title: Text(
                        file.name,
                        style: GoogleFonts.inter(
                          color: Colors.grey[300],
                          fontSize: 14,
                        ),
                      ),
                      subtitle: Text(
                        '${(file.size / 1024).toStringAsFixed(1)} KB',
                        style: GoogleFonts.inter(
                          color: Colors.grey[500],
                          fontSize: 12,
                        ),
                      ),
                      trailing: IconButton(
                        icon: const Icon(Icons.download, size: 20),
                        color: Colors.grey[400],
                        onPressed: () => downloadFile(file),
                      ),
                      onTap: () => _showFilePreview(file),
                    );
                  },
                ),
        ),
      ],
    );
  }

  void _showFilePreview(FileData file) {
    showDialog(
      context: context,
      builder: (context) => Dialog(
        child: Container(
          width: MediaQuery.of(context).size.width * 0.6,
          height: MediaQuery.of(context).size.height * 0.8,
          padding: const EdgeInsets.all(16),
          color: const Color(0xFF1E1E1E),
          child: Column(
            crossAxisAlignment: CrossAxisAlignment.stretch,
            children: [
              Row(
                children: [
                  Text(
                    file.name,
                    style: GoogleFonts.inter(
                      color: Colors.grey[300],
                      fontSize: 16,
                      fontWeight: FontWeight.w600,
                    ),
                  ),
                  const Spacer(),
                  IconButton(
                    icon: const Icon(Icons.close),
                    color: Colors.grey[400],
                    onPressed: () => Navigator.pop(context),
                  ),
                ],
              ),
              const SizedBox(height: 16),
              Expanded(
                child: Container(
                  padding: const EdgeInsets.all(16),
                  decoration: BoxDecoration(
                    color: const Color(0xFF252526),
                    borderRadius: BorderRadius.circular(8),
                  ),
                  child: SingleChildScrollView(
                    child: Text(
                      file.content,
                      style: GoogleFonts.firaCode(
                        color: Colors.grey[300],
                        fontSize: 14,
                      ),
                    ),
                  ),
                ),
              ),
            ],
          ),
        ),
      ),
    );
  }

  Widget _buildTerminalWindow() {
    return Container(
      decoration: BoxDecoration(
        color: const Color(0xFF1E1E1E),
        borderRadius: BorderRadius.circular(8),
        border: Border.all(
          color: Colors.grey.withOpacity(0.2),
          width: 1,
        ),
      ),
      margin: const EdgeInsets.all(16),
      child: Column(
        crossAxisAlignment: CrossAxisAlignment.stretch,
        children: [
          Container(
            padding: const EdgeInsets.symmetric(horizontal: 12, vertical: 8),
            decoration: BoxDecoration(
              color: Colors.black.withOpacity(0.3),
              borderRadius: const BorderRadius.vertical(top: Radius.circular(8)),
            ),
            child: Row(
              children: [
                ...['#FF5F56', '#FFBD2E', '#27C93F'].map((color) => Container(
                      width: 12,
                      height: 12,
                      margin: const EdgeInsets.only(right: 8),
                      decoration: BoxDecoration(
                        color: Color(
                            int.parse(color.substring(1), radix: 16) | 0xFF000000),
                        shape: BoxShape.circle,
                      ),
                    )),
                const Spacer(),
                Text(
                  'Terminal',
                  style: GoogleFonts.firaCode(
                    color: Colors.grey[400],
                    fontSize: 12,
                  ),
                ),
                const Spacer(),
              ],
            ),
          ),
          Expanded(
            child: SingleChildScrollView(
              padding: const EdgeInsets.all(16),
              child: Column(
                crossAxisAlignment: CrossAxisAlignment.stretch,
                children: terminalLogs
                    .map(
                      (log) => Padding(
                        padding: const EdgeInsets.only(bottom: 8),
                        child: Text(
                          log,
                          style: GoogleFonts.firaCode(
                            color: Colors.grey[300],
                            fontSize: 14,
                            height: 1.5,
                          ),
                        ),
                      ),
                    )
                    .toList(),
              ),
            ),
          ),
        ],
      ),
    );
  }

  Widget _buildStatusBar() {
    return Container(
      padding: const EdgeInsets.symmetric(horizontal: 16, vertical: 8),
      decoration: BoxDecoration(
        color: Colors.black.withOpacity(0.3),
      ),
      child: Row(
        children: [
          Text(
            'Status: ${isProcessing ? "Processing" : "Ready"}',
            style: GoogleFonts.inter(color: Colors.grey[400], fontSize: 12),
          ),
          const Spacer(),
          Text(
            'Temperature: $temperature',
            style: GoogleFonts.inter(color: Colors.grey[400], fontSize: 12),
          ),
          const SizedBox(width: 16),
          Text(
            'Max Length: $maxLength',
            style: GoogleFonts.inter(color: Colors.grey[400], fontSize: 12),
          ),
          const SizedBox(width: 16),
          Text(
            'Top P: $topP',
            style: GoogleFonts.inter(color: Colors.grey[400], fontSize: 12),
          ),
        ],
      ),
    );
  }

  @override
  Widget build(BuildContext context) {
    return Scaffold(
      backgroundColor: const Color(0xFF1E1E1E),
      body: Column(
        children: [
          Expanded(
            child: Row(
              children: [
                // Left side - Terminal
                Expanded(
                  flex: 2,
                  child: _buildTerminalWindow(),
                ),
                // Right side - Tabs
                Expanded(
                  flex: 1,
                  child: Container(
                    decoration: BoxDecoration(
                      color: const Color(0xFF252526),
                      border: Border(
                        left: BorderSide(
                          color: Colors.grey.withOpacity(0.2),
                          width: 1,
                        ),
                      ),
                    ),
                    child: Column(
                      children: [
                        TabBar(
                          controller: _tabController,
                          tabs: const [
                            Tab(text: 'Original Files'),
                            Tab(text: 'Converted Files'),
                          ],
                          labelStyle: GoogleFonts.inter(fontSize: 14),
                          labelColor: Colors.blue,
                          unselectedLabelColor: Colors.grey[400],
                          indicatorColor: Colors.blue,
                        ),
                        Expanded(
                          child: TabBarView(
                            controller: _tabController,
                            children: [
                              _buildFileList(
                                  originalFiles, 'Files for Conversion'),
                              _buildFileList(convertedFiles, 'Converted Files'),
                            ],
                          ),
                        ),
                      ],
                    ),
                  ),
                ),
              ],
            ),
          ),
          _buildStatusBar(),
        ],
      ),
    );
  }
}

class FileData {
  final String name;
  final String content;
  final int size;
  final String type;

  FileData({
    required this.name,
    required this.content,
    required this.size,
    required this.type,
  });
}