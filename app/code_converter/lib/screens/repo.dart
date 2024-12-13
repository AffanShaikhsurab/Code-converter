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
    return Theme(
      data: ThemeData.dark().copyWith(
        scaffoldBackgroundColor: const Color(0xFF1E1E1E),
        cardColor: const Color(0xFF2D2D2D),
        appBarTheme: const AppBarTheme(
          backgroundColor: Color(0xFF1E1E1E),
          elevation: 0,
        ),
        textTheme: GoogleFonts.interTextTheme(
          ThemeData.dark().textTheme,
        ),
      ),
      child: Scaffold(
        appBar: AppBar(
          title: Text(
            'Select Repository',
            style: GoogleFonts.inter(
              fontWeight: FontWeight.w600,
            ),
          ),
          actions: [
            IconButton(
              icon: const Icon(Icons.refresh, color: Colors.white70),
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
        body: Container(
          decoration: BoxDecoration(
            color: const Color(0xFF2D2D2D),
            borderRadius: BorderRadius.circular(12),
          ),
          margin: const EdgeInsets.all(16),
          child: Column(
            children: [
              Padding(
                padding: const EdgeInsets.all(16.0),
                child: TextField(
                  controller: searchController,
                  style: GoogleFonts.inter(),
                  decoration: InputDecoration(
                    hintText: 'Search repositories...',
                    hintStyle: GoogleFonts.inter(color: Colors.white54),
                    prefixIcon: const Icon(Icons.search, color: Colors.white54),
                    filled: true,
                    fillColor: const Color(0xFF1E1E1E),
                    border: OutlineInputBorder(
                      borderRadius: BorderRadius.circular(8),
                      borderSide: BorderSide.none,
                    ),
                    enabledBorder: OutlineInputBorder(
                      borderRadius: BorderRadius.circular(8),
                      borderSide: BorderSide.none,
                    ),
                    focusedBorder: OutlineInputBorder(
                      borderRadius: BorderRadius.circular(8),
                      borderSide: const BorderSide(color: Colors.blue, width: 1),
                    ),
                  ),
                  onChanged: (_) => setState(() {}),
                ),
              ),
              Expanded(
                child: Builder(
                  builder: (context) {
                    if (isLoading) {
                      return const Center(
                        child: CircularProgressIndicator(
                          color: Colors.blue,
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
                            Text(
                              error!,
                              style: GoogleFonts.inter(),
                            ),
                            const SizedBox(height: 16),
                            FilledButton(
                              style: ButtonStyle(
                                backgroundColor: MaterialStateProperty.all(Colors.blue),
                              ),
                              onPressed: () {
                                setState(() {
                                  isLoading = true;
                                  error = null;
                                });
                                fetchRepositories();
                              },
                              child: Text(
                                'Retry',
                                style: GoogleFonts.inter(),
                              ),
                            ),
                          ],
                        ),
                      );
                    }

                    final filteredRepos = getFilteredRepositories();
                    if (filteredRepos.isEmpty) {
                      return Center(
                        child: Text(
                          'No repositories found',
                          style: GoogleFonts.inter(color: Colors.white70),
                        ),
                      );
                    }

                    return ListView.builder(
                      itemCount: filteredRepos.length,
                      itemBuilder: (context, index) {
                        final repo = filteredRepos[index];
                        return Container(
                          margin: const EdgeInsets.symmetric(
                            horizontal: 16,
                            vertical: 8,
                          ),
                          decoration: BoxDecoration(
                            color: const Color(0xFF1E1E1E),
                            borderRadius: BorderRadius.circular(8),
                          ),
                          child: ListTile(
                            leading: const Icon(
                              Icons.folder_outlined,
                              color: Colors.white70,
                            ),
                            title: Text(
                              repo['full_name'],
                              style: GoogleFonts.inter(
                                fontWeight: FontWeight.w500,
                              ),
                            ),
                            subtitle: Text(
                              repo['description'] ?? 'No description',
                              style: GoogleFonts.inter(
                                color: Colors.white54,
                                fontSize: 13,
                              ),
                              maxLines: 2,
                              overflow: TextOverflow.ellipsis,
                            ),
                            trailing: const Icon(
                              Icons.chevron_right,
                              color: Colors.white70,
                            ),
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
                        ).animate()
                          .fadeIn(duration: const Duration(milliseconds: 200))
                          .slideX(begin: 0.2, duration: const Duration(milliseconds: 200));
                      },
                    );
                  },
                ),
              ),
            ],
          ),
        ),
      ),
    );
  }
}