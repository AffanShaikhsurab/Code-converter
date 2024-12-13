import 'package:shared_preferences/shared_preferences.dart';

class AuthService {
  static const String _tokenKey = 'github_access_token';

  // Initialization of SharedPreferences
  static Future<SharedPreferences> _getPreferences() async {
    try {
      return await SharedPreferences.getInstance();
    } catch (e) {
      print('Error initializing SharedPreferences: $e');
      rethrow;
    }
  }

  // Save the access token
  static Future<void> saveToken(String token) async {
    try {
      final prefs = await _getPreferences();
      await prefs.setString(_tokenKey, token);
    } catch (e) {
      print('Error saving token: $e');
      throw Exception('Failed to save authentication token');
    }
  }

  // Retrieve the access token
  static Future<String?> getToken() async {
    try {
      final prefs = await _getPreferences();
      return prefs.getString(_tokenKey);
    } catch (e) {
      print('Error retrieving token: $e');
      return null;
    }
  }

  // Remove the access token (logout)
  static Future<void> removeToken() async {
    try {
      final prefs = await _getPreferences();
      await prefs.remove(_tokenKey);
    } catch (e) {
      print('Error removing token: $e');
      throw Exception('Failed to remove authentication token');
    }
  }

  // Check if the user is authenticated
  static Future<bool> isAuthenticated() async {
    try {
      final token = await getToken();
      return token != null && token.isNotEmpty;
    } catch (e) {
      print('Error checking authentication: $e');
      return false;
    }
  }
}
