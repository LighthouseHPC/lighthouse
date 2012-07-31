#include <string>
using namespace std;

// set up temporary work space to store unique implementations
// and empricial testing needs
int setUpWorkSpace (string &out_file_name, string &fileName,
					string &path, string &tmpPath, string &pathToTop);

// move best version to friendly location and report
void handleBestVersion (string &fileName, string &path,
					string &tmpPath, int bestVersion);