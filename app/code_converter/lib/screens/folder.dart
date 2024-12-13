
import 'package:code_converter/screens/conversion.dart';
import 'package:code_converter/screens/github.dart';
import 'package:flutter/material.dart';
import 'package:http/http.dart' as http;
import 'dart:convert';
import 'package:url_launcher/url_launcher.dart';
import 'package:graphview/graphview.dart';
import 'package:flutter_animate/flutter_animate.dart';
import 'package:google_fonts/google_fonts.dart';
class CobolFolderScreen extends StatefulWidget {
  final String accessToken;
  final dynamic repository;

  const CobolFolderScreen({
    super.key,
    required this.accessToken,
    required this.repository,
  });

  @override
  State<CobolFolderScreen> createState() => _CobolFolderScreenState();
}

class _CobolFolderScreenState extends State<CobolFolderScreen> {
  List<FolderInfo> cobolFolders = [];
  bool isLoading = true;
  String? error;
  TextEditingController searchController = TextEditingController();
  bool isSearching = false;

  @override
  void initState() {
    super.initState();
    findCobolFolders();
  }

Future<void> findCobolFolders() async {
  setState(() {
    isLoading = true;
    error = null;
  });

  try {
    final response = await http.get(
      Uri.parse(widget.repository['contents_url']
          .toString()
          .replaceAll('{+path}', '')),
      headers: {
        'Authorization': 'Bearer ${widget.accessToken}',
        'Accept': 'application/json',
      },
    );

    if (response.statusCode == 200) {
      final contents = json.decode(response.body) as List<dynamic>;
      final folders = <FolderInfo>[];

      // Ensure that we pass a proper Iterable<Future> to Future.wait
      await Future.wait(
        contents
            .where((content) => content['type'] == 'dir')
            .map<Future<void>>((content) async {
              final hasCobol = await checkForCobolFiles(
                content['url'],
                content['path'],
              );
              if (hasCobol.isNotEmpty) {
                folders.add(FolderInfo(
                  path: content['path'],
                  cobolFiles: hasCobol,
                ));
              }
            })
            .toList(),
      );

      setState(() {
        cobolFolders = folders;
        isLoading = false;
      });
    } else {
      throw Exception('Failed to fetch repository contents');
    }
  } catch (e) {
    setState(() {
      error = e.toString();
      isLoading = false;
    });
  }
}


  Future<List<String>> checkForCobolFiles(String url, String path) async {
    try {
      final response = await http.get(
        Uri.parse(url),
        headers: {
          'Authorization': 'Bearer ${widget.accessToken}',
          'Accept': 'application/json',
        },
      );

      if (response.statusCode == 200) {
        final contents = json.decode(response.body);
        return contents
            .where((file) =>
                file['type'] == 'file' &&
                (file['name'].toLowerCase().endsWith('.cbl') ||
                    file['name'].toLowerCase().endsWith('.cobol')))
            .map<String>((file) => file['name'].toString())
            .toList();
      }
    } catch (e) {
      print('Error checking COBOL files in $path: $e');
    }
    return [];
  }

  List<FolderInfo> get filteredFolders {
    final query = searchController.text.toLowerCase();
    if (query.isEmpty) return cobolFolders;
    return cobolFolders
        .where((folder) => folder.path.toLowerCase().contains(query))
        .toList();
  }

  @override
  Widget build(BuildContext context) {
    return Scaffold(
      appBar: AppBar(
        title: isSearching
            ? TextField(
                controller: searchController,
                autofocus: true,
                decoration: InputDecoration(
                  hintText: 'Search folders...',
                  border: InputBorder.none,
                  hintStyle: TextStyle(
                    color: Theme.of(context).colorScheme.onSurface.withOpacity(0.5),
                  ),
                ),
                style: TextStyle(
                  color: Theme.of(context).colorScheme.onSurface,
                ),
                onChanged: (_) => setState(() {}),
              )
            : const Text('Select COBOL Folder'),
        actions: [
          IconButton(
            icon: Icon(isSearching ? Icons.close : Icons.search),
            onPressed: () {
              setState(() {
                if (isSearching) {
                  searchController.clear();
                }
                isSearching = !isSearching;
              });
            },
          ),
          IconButton(
            icon: const Icon(Icons.refresh),
            onPressed: isLoading ? null : findCobolFolders,
          ),
        ],
      ),
      body: RefreshIndicator(
        onRefresh: findCobolFolders,
        child: Builder(
          builder: (context) {
            if (isLoading) {
              return const Center(
                child: Column(
                  mainAxisAlignment: MainAxisAlignment.center,
                  children: [
                    CircularProgressIndicator(),
                    SizedBox(height: 16),
                    Text('Scanning repository for COBOL files...'),
                  ],
                ),
              );
            }

            if (error != null) {
              return Center(
                child: Column(
                  mainAxisAlignment: MainAxisAlignment.center,
                  children: [
                    Icon(
                      Icons.error_outline,
                      size: 48,
                      color: Theme.of(context).colorScheme.error,
                    ),
                    const SizedBox(height: 16),
                    Text(error!),
                    const SizedBox(height: 16),
                    FilledButton.icon(
                      onPressed: findCobolFolders,
                      icon: const Icon(Icons.refresh),
                      label: const Text('Retry'),
                    ),
                  ],
                ),
              );
            }

            if (cobolFolders.isEmpty) {
              return Center(
                child: Column(
                  mainAxisAlignment: MainAxisAlignment.center,
                  children: [
                    Icon(
                      Icons.folder_off,
                      size: 48,
                      color: Theme.of(context).colorScheme.outline,
                    ),
                    const SizedBox(height: 16),
                    const Text('No COBOL files found in this repository'),
                    const SizedBox(height: 8),
                    TextButton(
                      onPressed: () => Navigator.pop(context),
                      child: const Text('Select Another Repository'),
                    ),
                  ],
                ),
              );
            }

            final folders = filteredFolders;
            if (folders.isEmpty) {
              return const Center(
                child: Text('No matching folders found'),
              );
            }

            return ListView.builder(
              itemCount: folders.length,
              padding: const EdgeInsets.all(8),
              itemBuilder: (context, index) {
                final folder = folders[index];
                return Card(
                  margin: const EdgeInsets.symmetric(
                    horizontal: 8,
                    vertical: 4,
                  ),
                  child: ListTile(
                    leading: Badge(
                      label: Text(folder.cobolFiles.length.toString()),
                      child: const Icon(Icons.folder),
                    ),
                    title: Text(folder.path),
                    subtitle: Text(
                      'COBOL Files: ${folder.cobolFiles.join(", ")}',
                      maxLines: 2,
                      overflow: TextOverflow.ellipsis,
                    ),
                    trailing: const Icon(Icons.chevron_right),
                    onTap: () {
                      Navigator.push(
                        context,
                        MaterialPageRoute(
                          builder: (context) => ConversionScreen(
                            accessToken: widget.accessToken,
                            repository: widget.repository,
                            folderPath: folder.path,
                          ),
                        ),
                      );
                    },
                  ),
                ).animate().fadeIn(
                      delay: Duration(milliseconds: index * 50),
                    );
              },
            );
          },
        ),
      ),
    );
  }
}

class FolderInfo {
  final String path;
  final List<String> cobolFiles;

  FolderInfo({required this.path, required this.cobolFiles});
}