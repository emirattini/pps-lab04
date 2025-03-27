package u04.monads;

import javax.swing.*;
import java.awt.*;
import java.util.*;
import java.util.concurrent.*;
import java.util.function.*;
import u04.monads.Event;
import u04.monads.Event.*;

class SwingFunctionalFacade {

    public static interface Frame {
        Frame setSize(int width, int height);
        Frame addButton(String text, String name);
        Frame addButton(String text, String name, String eventInput);
        Frame addLabel(String text, String name);
        Frame addTextField(String name);
        Frame showToLabel(String text, String name);
        Frame show();
        Supplier<Event> events();
    }

    public static Frame createFrame(){
        return new FrameImpl();
    }

    /*
    private static class FrameImpl implements Frame {
      ...
    }
    */

    private static class FrameImpl implements Frame {
        private final JFrame jframe = new JFrame();
        private final Map<String, JButton> buttons = new HashMap<>();
        private final Map<String, JLabel> labels = new HashMap<>();
        private final Map<String, JTextField> textFields = new HashMap<>();
        private final LinkedBlockingQueue<Event> eventQueue = new LinkedBlockingQueue<>();
        private final Supplier<Event> events = () -> {
            try{
                return eventQueue.take();
            } catch (InterruptedException e){
                return new Event("", "");
            }
        };
        public FrameImpl() {
            this.jframe.setLayout(new FlowLayout());
        }

        @Override
        public Frame setSize(int width, int height) {
            this.jframe.setSize(width, height);
            return this;
        }

        @Override
        public Frame addButton(String text, String name) {
            return addButton(text, name, null);
        }

        @Override
        public Frame addButton(String text, String name, String eventInput) {
            JButton jb = new JButton(text);
            jb.setActionCommand(name);
            this.buttons.put(name, jb);
            jb.addActionListener(e -> {
                try {
                    if (eventInput != null) {
                        JTextField inputField = textFields.get(eventInput);
                        Integer number = Integer.parseInt(inputField.getText());
                        eventQueue.put(new Event(name, number.toString()));
                        inputField.setText("");
                    } else {
                        eventQueue.put(new Event(name, ""));
                    }
                } catch (InterruptedException ex){
                } catch (NumberFormatException ex) {
                    System.out.println("Not valid number");
                }
            });
            this.jframe.getContentPane().add(jb);
            return this;
        }

        @Override
        public Frame addLabel(String text, String name) {
            JLabel jl = new JLabel(text);
            this.labels.put(name, jl);
            this.jframe.getContentPane().add(jl);
            return this;
        }

        @Override
        public Frame addTextField(String name) {
            JTextField jtf = new JTextField();
            this.textFields.put(name, jtf);
            this.jframe.getContentPane().add(jtf);
            jtf.setPreferredSize(new Dimension(50, 25));
            return this;
        }

        @Override
        public Supplier<Event> events() {
            return events;
        }

        @Override
        public Frame showToLabel(String text, String name) {
            this.labels.get(name).setText(text);
            return this;
        }

        @Override
        public Frame show() {
            this.jframe.setVisible(true);
            return this;
        }

    }
}
