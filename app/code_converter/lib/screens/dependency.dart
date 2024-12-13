import 'package:code_converter/screens/github.dart';
import 'package:flutter/material.dart';
import 'package:http/http.dart' as http;
import 'dart:convert';
import 'package:url_launcher/url_launcher.dart';
import 'package:graphview/graphview.dart';
import 'package:flutter_animate/flutter_animate.dart';
import 'package:google_fonts/google_fonts.dart';

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
