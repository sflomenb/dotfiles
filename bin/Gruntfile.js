export const watcher = function(grunt) {
  let filePattern = grunt.option('pattern') || '**/*.py,!**/venv/**';
  filePattern = filePattern.split(',');
  const testDir = grunt.option('testDir') || '.';
  const commandToRun = grunt.option('command') || "python -m unittest discover -p '*_test.py'";

  grunt.initConfig({
    watch: {
      files: filePattern,
      tasks: ['shell']
    },
    shell: {
      test: {
        command: commandToRun,
        options: {
          execOptions: {
            cwd: testDir
          }
        }
      }
    }
  });

  grunt.loadNpmTasks('grunt-contrib-watch');
  grunt.loadNpmTasks('grunt-shell');

  grunt.registerTask('default', ['watch']);
};
