package expostatshorsligne;

import java.awt.BorderLayout;
import java.awt.event.ActionEvent;
import java.awt.event.ActionListener;
import javax.swing.JButton;
import javax.swing.JFrame;
import javax.swing.JPanel;

public class Launcher extends JPanel {
    
  private static JavaR hello;
  public static final String LANG_FR = "Fr";
  public static final String ARCH_X64 = "x64";
   
  public static void main(String[] args) {
    String lang = Launcher.LANG_FR;
        
    if ( args.length >= 1 ) {
      lang = args[0];
    }
    
    JButton quit = new JButton(lang.equals(Launcher.LANG_FR) ? "Quitter" : "Quit");
    quit.setSize(100,150);
    quit.addActionListener(new ActionListener() {
      public void actionPerformed(ActionEvent e) {
          hello.end();
          System.exit(0);
      }
    });

    JFrame f = new JFrame("ExpostatsTools");
    f.setSize(300, 300);
    f.setDefaultCloseOperation(JFrame.EXIT_ON_CLOSE);
    JPanel p = new JPanel();
    p.add(quit);
    f.getContentPane().add(p, BorderLayout.SOUTH);
    f.pack();
    f.setSize(300,100);
    f.setVisible(true);
    
    hello = new JavaR(lang);
  }
}