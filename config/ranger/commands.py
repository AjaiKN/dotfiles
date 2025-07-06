from ranger.api.commands import Command

# from https://github.com/bulletmark/edir?tab=readme-ov-file#embed-in-ranger-file-manager
class edir(Command):
    '''
    :edir [file|dir]

    Run edir on the selected file or dir.
    Default argument is current dir.
    '''
    def execute(self):
        self.fm.run('edir -q ' + self.rest(1))
    def tab(self, tabnum):
        return self._tab_directory_content()
