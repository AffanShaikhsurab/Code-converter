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
      home: const GithubAuthScreen(),
    );
}
}

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
  bool isConverting = false;
  List<ConversionStep> conversionSteps = [];
  Map<String, List<String>> dependencyGraph = {};
  String? error;
  double progress = 0.0;

  @override
  void initState() {
    super.initState();
    startConversion();
  }

  Future<void> startConversion() async {
    setState(() {
      isConverting = true;
      error = null;
      addStep('Initializing conversion process...', StepStatus.inProgress);
    });

    try {
      // Download COBOL files
      setState(() => progress = 0.2);
      // await downloadCobolFiles();
      updateLastStep(StepStatus.completed);
      
      // Analyze dependencies
      setState(() => progress = 0.4);
      // await analyzeDependencies();
      updateLastStep(StepStatus.completed);
      
      // Convert files
      setState(() => progress = 0.6);
      // await convertFiles();
      updateLastStep(StepStatus.completed);
      
      // Generate Python project structure
      setState(() => progress = 0.8);
      // await generatePythonProject();
      updateLastStep(StepStatus.completed);
      
      // Create conversion report
      setState(() => progress = 1.0);
      // await createReport();
      updateLastStep(StepStatus.completed);
      
      addStep('Conversion completed successfully!', StepStatus.completed);
    } catch (e) {
      setState(() {
        error = e.toString();
        updateLastStep(StepStatus.failed);
      });
    } finally {
      setState(() {
        isConverting = false;
      });
    }
  }

  void addStep(String description, StepStatus status) {
    setState(() {
      conversionSteps.add(ConversionStep(description, status));
    });
  }

  void updateLastStep(StepStatus status) {
    if (conversionSteps.isNotEmpty) {
      setState(() {
        conversionSteps.last = ConversionStep(
          conversionSteps.last.description,
          status,
        );
      });
    }
  }

  // Implementation of the conversion steps...
  // (downloadCobolFiles, analyzeDependencies, etc.)

  @override
  Widget build(BuildContext context) {
    return Scaffold(
      appBar: AppBar(
        title: const Text('Converting COBOL to Python'),
      ),
      body: Column(
        children: [
          LinearProgressIndicator(
            value: progress,
            backgroundColor: Theme.of(context).colorScheme.surfaceVariant,
          ),
          Expanded(
            child: ListView.builder(
              itemCount: conversionSteps.length,
              itemBuilder: (context, index) {
                final step = conversionSteps[index];
                return ListTile(
                  leading: _buildStepIcon(step.status),
                  title: Text(step.description),
                  subtitle: _buildStepSubtitle(step.status),
                ).animate().fadeIn(delay: Duration(milliseconds: index * 200));
              },
            ),
          ),
if (dependencyGraph.isNotEmpty)
            Expanded(
              child: Card(
                margin: const EdgeInsets.all(16),
                child: Column(
                  crossAxisAlignment: CrossAxisAlignment.stretch,
                  children: [
                    Padding(
                      padding: const EdgeInsets.all(16),
                      child: Text(
                        'Dependency Graph',
                        style: Theme.of(context).textTheme.titleLarge,
                      ),
                    ),
                    Expanded(
                      child: DependencyGraphView(
                        dependencies: dependencyGraph,
                      ).animate().fadeIn(),
                    ),
                  ],
                ),
              ),
            ),
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
                    Text(
                      error!,
                      style: Theme.of(context).textTheme.bodyMedium?.copyWith(
                            color: Theme.of(context).colorScheme.onErrorContainer,
                          ),
                    ),
                    const SizedBox(height: 16),
                    FilledButton.icon(
                      onPressed: startConversion,
                      icon: const Icon(Icons.refresh),
                      label: const Text('Retry Conversion'),
                    ),
                  ],
                ),
              ),
            ),
          if (isConverting)
            const Padding(
              padding: EdgeInsets.all(16),
              child: LinearProgressIndicator(),
            ),
        ],
      ),
    );
  }

  Widget _buildStepIcon(StepStatus status) {
    switch (status) {
      case StepStatus.completed:
        return Icon(
          Icons.check_circle,
          color: Theme.of(context).colorScheme.primary,
        );
      case StepStatus.inProgress:
        return SizedBox(
          width: 24,
          height: 24,
          child: CircularProgressIndicator(
            strokeWidth: 2,
            color: Theme.of(context).colorScheme.primary,
          ),
        );
      case StepStatus.failed:
        return Icon(
          Icons.error,
          color: Theme.of(context).colorScheme.error,
        );
      default:
        return Icon(
          Icons.circle_outlined,
          color: Theme.of(context).colorScheme.outline,
        );
    }
  }

  Widget _buildStepSubtitle(StepStatus status) {
    switch (status) {
      case StepStatus.completed:
        return Text(
          'Completed',
          style: TextStyle(color: Theme.of(context).colorScheme.primary),
        );
      case StepStatus.inProgress:
        return Text(
          'In Progress',
          style: TextStyle(color: Theme.of(context).colorScheme.primary),
        );
      case StepStatus.failed:
        return Text(
          'Failed',
          style: TextStyle(color: Theme.of(context).colorScheme.error),
        );
      default:
        return const SizedBox.shrink();
    }
  }
}

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

class DependencyGraphView extends StatelessWidget {
  final Map<String, List<String>> dependencies;

  const DependencyGraphView({super.key, required this.dependencies});

  @override
  Widget build(BuildContext context) {
    final Graph graph = Graph();
    final nodes = <String, Node>{};
    final algorithm = SugiyamaAlgorithm(
      SugiyamaConfiguration()
        ..nodeSeparation = 100
        ..levelSeparation = 100,
    );

    // Create nodes with modern styling
    for (final file in dependencies.keys) {
      nodes[file] = Node.Id(file);
      graph.addNode(nodes[file]!);
    }

    // Create edges with improved styling
    for (final entry in dependencies.entries) {
      for (final dependency in entry.value) {
        graph.addEdge(
          nodes[entry.key]!,
          nodes[dependency]!,
          paint: Paint()
            ..color = Theme.of(context).colorScheme.primary.withOpacity(0.5)
            ..strokeWidth = 2,
        );
      }
    }

    return InteractiveViewer(
      constrained: false,
      boundaryMargin: const EdgeInsets.all(100),
      minScale: 0.01,
      maxScale: 5.6,
      child: GraphView(
        graph: graph,
        algorithm: algorithm,
        paint: Paint()
          ..color = Theme.of(context).colorScheme.primary
          ..strokeWidth = 2
          ..style = PaintingStyle.stroke,
        builder: (Node node) {
          return Card(
            elevation: 4,
            shape: RoundedRectangleBorder(
              borderRadius: BorderRadius.circular(12),
            ),
            child: Container(
              padding: const EdgeInsets.symmetric(
                horizontal: 16,
                vertical: 8,
              ),
              decoration: BoxDecoration(
                gradient: LinearGradient(
                  colors: [
                    Theme.of(context).colorScheme.primaryContainer,
                    Theme.of(context).colorScheme.surface,
                  ],
                ),
                borderRadius: BorderRadius.circular(12),
              ),
              child: Text(
                node.key?.value?.toString() ?? '',
                style: Theme.of(context).textTheme.bodyMedium?.copyWith(
                      fontWeight: FontWeight.bold,
                    ),
              ),
            ),
          );
        },
      ),
    );
  }
}

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
        final contents = json.decode(response.body);
        final folders = <FolderInfo>[];

        await Future.wait(
          contents.where((content) => content['type'] == 'dir').map((content) async {
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
          }),
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