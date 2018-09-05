from os import kill
from signal import alarm, signal, SIGALRM, SIGKILL
from subprocess import PIPE, Popen

class Alarm(Exception):
    pass

class Command(object):
    def __init__(self, cmd):
        self.cmd = cmd
        self.process = None

    # Doesn't seem to work on ix
    def run_darwin(self, inputs, timeout=-1):
        import threading
        out = err = ''
        def target():
            import subprocess
            self.process = subprocess.Popen(self.cmd, shell=True, stdin=subprocess.PIPE)
            if inputs: self.process.stdin.write(inputs)
            
            # Waits for the process to exit
            out, err = self.process.communicate()

        thread = threading.Thread(target=target)
        thread.start()

        if timeout>0: thread.join(timeout)
        if thread.is_alive():
            self.process.terminate()
            thread.join()

        return self.process.returncode, out, err
    
    
    def run(self, inputs='', timeout = -1, cwd = None, shell = True, kill_tree = True, env = None):
        '''
        Run a command with a timeout after which it will be forcibly
        killed.
        '''

        def alarm_handler(signum, frame):
            raise Alarm
        stdout = ''
        stderr = ''
        p = Popen(self.cmd, shell = shell, cwd = cwd, stdin=PIPE, stdout = PIPE, stderr = PIPE, env = env)
        if inputs: p.stdin.write(inputs)
        if timeout != -1:
            signal(SIGALRM, alarm_handler)
            alarm(timeout)
        try:
            stdout, stderr = p.communicate()
            if timeout != -1:
                alarm(0)
        except Alarm:
            pids = [p.pid]
            #stdout, stderr = p.communicate()
            if kill_tree:
                pids.extend(self.get_process_children(p.pid))
            for pid in pids:
                # process might have died before getting to this line
                # so wrap to avoid OSError: no such process
                try: 
                    kill(pid, SIGKILL)
                except OSError:
                    pass
            return -9, stdout, stderr + "Killed!"
        return p.returncode, stdout, stderr
    
    def get_process_children(self, pid):
        p = Popen('ps --no-headers -o pid --ppid %d' % pid, shell = True,
                  stdout = PIPE, stderr = PIPE)
        stdout, stderr = p.communicate()
        return [int(p) for p in stdout.split()]

if __name__ == '__main__':
    print Command('echo "Blah.\n"; sleep 2').run('', shell = True, timeout = 3)
    print Command('echo "Gah!\n"; sleep 5').run('', shell = True, timeout = 3)    

