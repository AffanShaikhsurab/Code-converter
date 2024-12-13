import 'package:code_converter/screens/folder.dart';
import 'package:code_converter/screens/github.dart';
import 'package:flutter/material.dart';
import 'package:http/http.dart' as http;
import 'dart:convert';
import 'package:url_launcher/url_launcher.dart';
import 'package:graphview/graphview.dart';
import 'package:flutter_animate/flutter_animate.dart';
import 'package:google_fonts/google_fonts.dart';
class RepositoryListScreen extends StatefulWidget {
  final String accessToken;

  const RepositoryListScreen({super.key, required this.accessToken});

  @override
  State<RepositoryListScreen> createState() => _RepositoryListScreenState();
}

class _RepositoryListScreenState extends State<RepositoryListScreen> {
  List<dynamic> repositories = [];
  bool isLoading = true;
  String? error;
  TextEditingController searchController = TextEditingController();

  @override
  void initState() {
    super.initState();
    fetchRepositories();
  }

  Future<void> fetchRepositories() async {
    try {
      final response = await http.get(
        Uri.parse('https://api.github.com/user/repos?sort=updated'),
        headers: {
          'Authorization': 'Bearer ${widget.accessToken}',
          'Accept': 'application/json',
        },
      );

      if (response.statusCode == 200) {
        setState(() {
          repositories = json.decode(response.body);
          isLoading = false;
        });
      } else {
        throw Exception('Failed to fetch repositories');
      }
    } catch (e) {
      setState(() {
        error = e.toString();
        isLoading = false;
      });
    }
  }

  List<dynamic> getFilteredRepositories() {
    final query = searchController.text.toLowerCase();
    return repositories.where((repo) =>
        repo['full_name'].toString().toLowerCase().contains(query)).toList();
  }

  @override
  Widget build(BuildContext context) {
    return Scaffold(
      appBar: AppBar(
        title: const Text('Select Repository'),
        actions: [
          IconButton(
            icon: const Icon(Icons.refresh),
            onPressed: () {
              setState(() {
                isLoading = true;
                error = null;
              });
              fetchRepositories();
            },
          ),
        ],
      ),
      body: Column(
        children: [
          Padding(
            padding: const EdgeInsets.all(16.0),
            child: TextField(
              controller: searchController,
              decoration: InputDecoration(
                hintText: 'Search repositories...',
                prefixIcon: const Icon(Icons.search),
                border: OutlineInputBorder(
                  borderRadius: BorderRadius.circular(12),
                ),
              ),
              onChanged: (_) => setState(() {}),
            ),
          ),
          Expanded(
            child: Builder(
              builder: (context) {
                if (isLoading) {
                  return const Center(child: CircularProgressIndicator());
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
                        FilledButton(
                          onPressed: () {
                            setState(() {
                              isLoading = true;
                              error = null;
                            });
                            fetchRepositories();
                          },
                          child: const Text('Retry'),
                        ),
                      ],
                    ),
                  );
                }

                final filteredRepos = getFilteredRepositories();
                if (filteredRepos.isEmpty) {
                  return const Center(
                    child: Text('No repositories found'),
                  );
                }

                return ListView.builder(
                  itemCount: filteredRepos.length,
                  itemBuilder: (context, index) {
                    final repo = filteredRepos[index];
                    return Card(
                      margin: const EdgeInsets.symmetric(
                        horizontal: 16,
                        vertical: 8,
                      ),
                      child: ListTile(
                        leading: const Icon(Icons.folder_outlined),
                        title: Text(repo['full_name']),
                        subtitle: Text(
                          repo['description'] ?? 'No description',
                          maxLines: 2,
                          overflow: TextOverflow.ellipsis,
                        ),
                        trailing: const Icon(Icons.chevron_right),
                        onTap: () {
                          Navigator.push(
                            context,
                            MaterialPageRoute(
                              builder: (context) => CobolFolderScreen(
                                accessToken: widget.accessToken,
                                repository: repo,
                              ),
                            ),
                          );
                        },
                      ),
                    );
                  },
                );
              },
            ),
          ),
        ],
      ),
    );
  }
}
