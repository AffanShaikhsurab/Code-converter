import 'package:flutter/material.dart';
import 'package:google_fonts/google_fonts.dart';
import 'package:http/http.dart' as http;
import 'dart:convert';
import 'package:shared_preferences/shared_preferences.dart';

class DashboardScreen extends StatefulWidget {
  final String? accessToken;
  
  const DashboardScreen({Key? key, this.accessToken}) : super(key: key);

  @override
  State<DashboardScreen> createState() => _DashboardScreenState();
}

class _DashboardScreenState extends State<DashboardScreen> {
  int filesConverted = 0;
  int conversionCount = 0;
  List<Map<String, dynamic>> recentConversions = [];
  List<Map<String, dynamic>> repositories = [];

  @override
  void initState() {
    super.initState();
    _loadStatistics();
    if (widget.accessToken != null) {
      _fetchGithubRepos();
    }
  }

  Future<void> _loadStatistics() async {
    final prefs = await SharedPreferences.getInstance();
    setState(() {
      filesConverted = prefs.getInt('filesConverted') ?? 0;
      conversionCount = prefs.getInt('conversionCount') ?? 0;
      final savedConversions = prefs.getStringList('recentConversions') ?? [];
      recentConversions = savedConversions
          .map((str) => json.decode(str) as Map<String, dynamic>)
          .toList();
    });
  }

  Future<void> _fetchGithubRepos() async {
    try {
      final response = await http.get(
        Uri.parse('https://api.github.com/user/repos'),
        headers: {'Authorization': 'Bearer ${widget.accessToken}'},
      );
      if (response.statusCode == 200) {
        setState(() {
          repositories = List<Map<String, dynamic>>.from(json.decode(response.body));
        });
      }
    } catch (e) {
      debugPrint('Error fetching repositories: $e');
    }
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
              Colors.grey[900]!,
              Colors.grey[850]!,
              Colors.grey[800]!,
            ],
          ),
        ),
        child: SafeArea(
          child: Row(
            children: [
              // Sidebar
              NavigationRail(
                backgroundColor: Colors.black54,
                selectedIndex: 0,
                onDestinationSelected: (int index) {},
                destinations: const [
                  NavigationRailDestination(
                    icon: Icon(Icons.dashboard),
                    label: Text('Dashboard'),
                  ),
                  NavigationRailDestination(
                    icon: Icon(Icons.history),
                    label: Text('History'),
                  ),
                  NavigationRailDestination(
                    icon: Icon(Icons.settings),
                    label: Text('Settings'),
                  ),
                ],
              ),
              // Main Content
              Expanded(
                child: Padding(
                  padding: const EdgeInsets.all(24.0),
                  child: Column(
                    crossAxisAlignment: CrossAxisAlignment.start,
                    children: [
                      Text(
                        'Dashboard',
                        style: GoogleFonts.inter(
                          fontSize: 32,
                          fontWeight: FontWeight.bold,
                          color: Colors.white,
                        ),
                      ),
                      const SizedBox(height: 32),
                      // Stats Cards
                      Row(
                        children: [
                          _buildStatCard(
                            'Files Converted',
                            filesConverted.toString(),
                            Icons.file_copy,
                          ),
                          const SizedBox(width: 16),
                          _buildStatCard(
                            'Total Conversions',
                            conversionCount.toString(),
                            Icons.loop,
                          ),
                          const SizedBox(width: 16),
                          _buildStatCard(
                            'Success Rate',
                            '95%',
                            Icons.check_circle,
                          ),
                        ],
                      ),
                      const SizedBox(height: 32),
                      // Recent Conversions and Repositories
                      Expanded(
                        child: Row(
                          crossAxisAlignment: CrossAxisAlignment.start,
                          children: [
                            Expanded(
                              child: _buildRecentConversions(),
                            ),
                            const SizedBox(width: 24),
                            Expanded(
                              child: _buildGithubRepos(),
                            ),
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
      ),
    );
  }

  Widget _buildStatCard(String title, String value, IconData icon) {
    return Expanded(
      child: Container(
        padding: const EdgeInsets.all(24),
        decoration: BoxDecoration(
          color: Colors.white.withOpacity(0.1),
          borderRadius: BorderRadius.circular(16),
          boxShadow: [
            BoxShadow(
              color: Colors.black.withOpacity(0.1),
              blurRadius: 10,
              offset: const Offset(0, 4),
            ),
          ],
        ),
        child: Column(
          crossAxisAlignment: CrossAxisAlignment.start,
          children: [
            Icon(icon, color: Colors.blue[400], size: 32),
            const SizedBox(height: 16),
            Text(
              title,
              style: GoogleFonts.inter(
                color: Colors.grey[400],
                fontSize: 14,
              ),
            ),
            const SizedBox(height: 8),
            Text(
              value,
              style: GoogleFonts.inter(
                color: Colors.white,
                fontSize: 24,
                fontWeight: FontWeight.bold,
              ),
            ),
          ],
        ),
      ),
    );
  }

  Widget _buildRecentConversions() {
    return Container(
      padding: const EdgeInsets.all(24),
      decoration: BoxDecoration(
        color: Colors.white.withOpacity(0.1),
        borderRadius: BorderRadius.circular(16),
      ),
      child: Column(
        crossAxisAlignment: CrossAxisAlignment.start,
        children: [
          Text(
            'Recent Conversions',
            style: GoogleFonts.inter(
              fontSize: 20,
              fontWeight: FontWeight.bold,
              color: Colors.white,
            ),
          ),
          const SizedBox(height: 16),
          Expanded(
            child: ListView.builder(
              itemCount: recentConversions.length,
              itemBuilder: (context, index) {
                final conversion = recentConversions[index];
                return ListTile(
                  leading: const Icon(Icons.file_present),
                  title: Text(
                    conversion['fileName'] ?? '',
                    style: const TextStyle(color: Colors.white),
                  ),
                  subtitle: Text(
                    conversion['date'] ?? '',
                    style: TextStyle(color: Colors.grey[400]),
                  ),
                );
              },
            ),
          ),
        ],
      ),
    );
  }

  Widget _buildGithubRepos() {
    return Container(
      padding: const EdgeInsets.all(24),
      decoration: BoxDecoration(
        color: Colors.white.withOpacity(0.1),
        borderRadius: BorderRadius.circular(16),
      ),
      child: Column(
        crossAxisAlignment: CrossAxisAlignment.start,
        children: [
          Text(
            'GitHub Repositories',
            style: GoogleFonts.inter(
              fontSize: 20,
              fontWeight: FontWeight.bold,
              color: Colors.white,
            ),
          ),
          const SizedBox(height: 16),
          Expanded(
            child: ListView.builder(
              itemCount: repositories.length,
              itemBuilder: (context, index) {
                final repo = repositories[index];
                return ListTile(
                  leading: const Icon(Icons.code, color: Colors.white),
                  title: Text(
                    repo['name'] ?? '',
                    style: const TextStyle(color: Colors.white),
                  ),
                  subtitle: Text(
                    repo['description'] ?? '',
                    style: TextStyle(color: Colors.grey[400]),
                  ),
                );
              },
            ),
          ),
        ],
      ),
    );
  }
}

