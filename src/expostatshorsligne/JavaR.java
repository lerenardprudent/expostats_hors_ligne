package expostatshorsligne;

import java.io.BufferedReader;
import java.io.IOException;
import java.io.InputStreamReader;
import java.util.regex.Matcher;
import java.util.regex.Pattern;
import org.rosuda.JRI.Rengine;

public class JavaR  {
  private final String R_base = "vendor/R";
  private static final Rengine engine = new Rengine(new String[] { "--vanilla" }, false, null);       
  private Process p = null, p2 = null, p3 = null;
  private String rPids[] = new String[10];
  private final String RLaunchScript = "start.R";
  
  public JavaR() {
    this(Launcher.LANG_FR);
  }
  
  public JavaR(String lang) {
    String cmd =  "\"" + R_base.replace("/", "\\") + "\\bin\\" + Launcher.ARCH_X64 + "\\R.exe\" -e \"env <- new.env(); env$toolName <- '%1s'; env$port <- %2s; sys.source('src/tools/" + RLaunchScript + "', env)\"";

    try {
      Runtime runtime = Runtime.getRuntime();
      for ( int i = 0; i <= 3; i++ ) {
        String toolDir = "tool" + i + lang;
        String runCmd = String.format(cmd, new Object[]{toolDir, Integer.toString(5555+i)});
        //System.out.println("==== " + runCmd + " ======");
        p = runtime.exec(runCmd);
        Thread.sleep(350);
        StreamGobbler errorGobbler = new 
        StreamGobbler(p.getErrorStream(), "ERROR");            
        StreamGobbler outputGobbler = new StreamGobbler(p.getInputStream(), "OUTPUT");
                
        // kick them off
        errorGobbler.start();
        outputGobbler.start();
                                    
        // any error???
        //int exitVal = p.waitFor();
        //System.out.println("ExitValue: " + exitVal);
      }
    } catch ( Exception e ) {
        System.err.println("Runtime error");
        e.printStackTrace();
    }

    try {
        String RtermExecName = "Rterm";
        cmd = "c:\\windows\\system32\\WindowsPowerShell\\v1.0\\powershell.exe \"Get-Process " + RtermExecName + " | Format-List Path,Id\"";
        p2 = Runtime.getRuntime().exec(cmd);
        p2.getOutputStream().close();
        String line;
        //System.out.println("Standard Output:");
        BufferedReader stdout = new BufferedReader(new InputStreamReader(p2.getInputStream()));
        String patternPath = "(?i:.*vendor.*" + RtermExecName + ".exe)";
        String linePattern = "^(Path|Id|)(\\s+:?\\s*)(.+)$";

        // Create a Pattern object
        Pattern regexp = Pattern.compile(linePattern);
        int c = 0;

        String path = "";
        String pid;
        while ((line = stdout.readLine()) != null) {
            Matcher m = regexp.matcher(line);
            Boolean match = m.matches();
            if ( match ) {
                String lineId = m.group(1);
                String lineContents = m.group(3);
                if ( lineId.equals("Path") ) {
                    path = lineContents;
                } else
                if ( lineId.equals("Id") ) {
                    pid = lineContents;
                    if ( path.matches(patternPath) ) {
                        rPids[c++] = pid;
                    }
                    path = "";
                } else
                if ( path.length() > 0 ) {
                    path += lineContents;
                }
            }
        }
        stdout.close();
    } catch ( IOException e ) {
        System.err.println("Runtime error");
        e.printStackTrace();
    }
  }

  public void finalize()
  {
    this.end();
  }

  public void end()
  {    
    try {
      if ( p != null ) {
        p.destroyForcibly();
      }

      for ( int d = 0; d < rPids.length; d++ ) {
        if ( rPids[d] != null ) {
          String cmd = "c:\\windows\\System32\\taskkill /PID " + rPids[d] + " /F";
          Runtime.getRuntime().exec(cmd);
        }
      }

      engine.end();
    } catch (Exception e) {
      System.err.println("Cleanup error");
    }
  }
}