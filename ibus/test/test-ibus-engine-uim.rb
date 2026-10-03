#!/usr/bin/env ruby
#
# Copyright (c) 2026 uim Project https://github.com/uim/uim
#
# All rights reserved.
#
# Redistribution and use in source and binary forms, with or without
# modification, are permitted provided that the following conditions
# are met:
#
# 1. Redistributions of source code must retain the above copyright
#    notice, this list of conditions and the following disclaimer.
# 2. Redistributions in binary form must reproduce the above copyright
#    notice, this list of conditions and the following disclaimer in the
#    documentation and/or other materials provided with the distribution.
# 3. Neither the name of authors nor the names of its contributors
#    may be used to endorse or promote products derived from this software
#    without specific prior written permission.
#
# THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS ``AS
# IS'' AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO,
# THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR
# PURPOSE ARE DISCLAIMED.  IN NO EVENT SHALL THE COPYRIGHT HOLDERS OR
# CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL,
# EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT LIMITED TO,
# PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF USE, DATA, OR PROFITS;
# OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY,
# WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR
# OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF THIS SOFTWARE, EVEN IF
# ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.

# Runs ibus-daemon on an address of its own, with a component
# directory that has only uim in it, and talks to it as GNOME Shell
# does, through libibus.
#
# Run this with run.rb, which loads IBus, under dbus-run-session with
# IBUS_ENGINE_UIM set. Put IBUS_UIM_DEBUG in the environment to see
# what the engine makes of the keys.

require "fileutils"
require "socket"
require "tmpdir"

# X keysyms, evdev key codes and IBus modifiers.
module Keyboard
  KEYVAL_A = 0x61
  KEYVAL_B = 0x62
  KEYVAL_J = 0x6a
  KEYVAL_K = 0x6b
  KEYVAL_L = 0x6c
  KEYVAL_N = 0x6e
  KEYVAL_X = 0x78
  KEYVAL_RETURN = 0xff0d
  KEYVAL_SHIFT_L = 0xffe1
  KEYVAL_CONTROL_L = 0xffe3
  KEYCODE_A = 30
  KEYCODE_B = 48
  KEYCODE_J = 36
  KEYCODE_K = 37
  KEYCODE_L = 38
  KEYCODE_N = 49
  KEYCODE_X = 45
  KEYCODE_LEFTCTRL = 29
  KEYCODE_LEFTSHIFT = 42

  CONTROL_MASK = IBus::ModifierType::CONTROL_MASK.to_i
  RELEASE_MASK = IBus::ModifierType::RELEASE_MASK.to_i
end

# Stands where uim-helper-server stands: the engine connects to it on
# its own, and the test reads what the engine tells the toolbar and
# sends what the toolbar would.
class HelperServer
  # Without the empty line that ends each of them.
  attr_reader :messages

  def initialize(path)
    @server = UNIXServer.new(path)
    @clients = {}
    @messages = []
  end

  def close
    disconnect
    @server.close
  end

  # As if uim-helper-server went away.
  def disconnect
    @clients.each_key(&:close)
    @clients.clear
  end

  # A message is lines, each ending with a newline.
  def send_message(message)
    @clients.each_key do |client|
      client.write("#{message}\n")
    end
  end

  def receive
    loop do
      client = @server.accept_nonblock(exception: false)
      break if client == :wait_readable
      @clients[client] = +""
    end
    @clients.each do |client, buffer|
      loop do
        data = client.read_nonblock(4096, exception: false)
        break if data == :wait_readable or data.nil?
        buffer << data
      end
      while (index = buffer.index("\n\n"))
        message = buffer.slice!(0, index + 2)
        @messages << message.chomp("\n").force_encoding("UTF-8")
      end
    end
  end
end

class IBusSession
  include Keyboard

  TIMEOUT = 10

  SERVICE = "org.freedesktop.IBus"
  PATH = "/org/freedesktop/IBus"
  PANEL_SERVICE = "org.freedesktop.IBus.Panel"

  # A copy of an IBus::Property: ibus-daemon's own go away after the
  # signal.
  Property = Struct.new(:key, :type, :label, :symbol, :state, :visible,
                        :sub_props)

  # What the engine sent back, one string each:
  #
  #   commit TEXT
  #   preedit CURSOR VISIBLE TEXT
  #   lookup-table CURSOR CURSOR_VISIBLE PAGE_SIZE CANDIDATE...
  #   hide-lookup-table
  attr_reader :events
  attr_reader :helper
  # What the panel shows, Property each.
  attr_reader :properties

  def initialize(default_im_name)
    @default_im_name = default_im_name
    @dir = nil
    @daemon = nil
    @connection = nil
    @context = nil
    @helper = nil
    @panel = nil
    @properties = nil
    @events = []
  end

  def start
    engine = ENV["IBUS_ENGINE_UIM"]
    raise("IBUS_ENGINE_UIM is not set") if engine.nil?

    @dir = Dir.mktmpdir("ibus-engine-uim-test")
    component_dir = File.join(@dir, "component")
    FileUtils.mkdir_p(component_dir)
    File.write(File.join(component_dir, "uim.xml"), component_xml(engine))
    scm_file = File.join(@dir, "user.scm")
    File.write(scm_file, user_scm)
    socket = File.join(@dir, "bus")
    address = "unix:path=#{socket}"
    runtime_dir = File.join(@dir, "runtime")
    helper_dir = File.join(runtime_dir, "uim", "socket")
    # libuim wants these directories to be the user's only.
    FileUtils.mkdir_p(helper_dir, mode: 0700)
    @helper = HelperServer.new(File.join(helper_dir, "uim-helper"))

    environment = {
      "IBUS_COMPONENT_PATH" => component_dir,
      "LIBUIM_USER_SCM_FILE" => scm_file,
      "HOME" => @dir,
      "XDG_CONFIG_HOME" => File.join(@dir, "config"),
      "XDG_CACHE_HOME" => File.join(@dir, "cache"),
      # Where the engine looks for uim-helper-server.
      "XDG_RUNTIME_DIR" => runtime_dir,
      # The desktop's settings stay out of the test.
      "GSETTINGS_BACKEND" => "memory",
      # The labels stay untranslated even with uim installed.
      "LC_ALL" => "C.UTF-8",
      "DISPLAY" => nil,
      "WAYLAND_DISPLAY" => nil,
    }
    command_line = [
      ENV["IBUS_DAEMON"] || "ibus-daemon",
      "--panel=disable",
      "--emoji-extension=disable",
      "--config=disable",
      "--cache=none",
      "--address=#{address}",
    ]
    # ibus-daemon throws away what the engine writes unless it is
    # verbose.
    command_line << "--verbose" if ENV["IBUS_UIM_DEBUG"]
    @daemon = Process.spawn(environment, *command_line)
    wait_for {File.exist?(socket)}
    raise("ibus-daemon didn't listen on #{socket}") unless File.exist?(socket)

    # Not IBus::Bus: there is only one of it in a process, and it
    # can't move on to the next test's ibus-daemon.
    flags = Gio::DBusConnectionFlags::AUTHENTICATION_CLIENT |
            Gio::DBusConnectionFlags::MESSAGE_BUS_CONNECTION
    @connection = Gio::DBusConnection.new(address, flags, nil, nil)
    start_panel
    reply = call(PATH, SERVICE, "CreateInputContext",
                 GLib::Variant.parse('("test",)'))
    @context = IBus::InputContext.new(reply[0], @connection, nil)
    connect_signals

    # The client asks for the lookup table itself, so ibus-daemon sends
    # it here instead of to a panel.
    @context.capabilities = IBus::Capabilite::PREEDIT_TEXT |
                            IBus::Capabilite::LOOKUP_TABLE |
                            IBus::Capabilite::FOCUS
    @context.focus_in
    # As GNOME Shell does. ibus-daemon replies once the engine is up and
    # attached to the focused context.
    call(PATH, SERVICE, "SetGlobalEngine", GLib::Variant.parse('("uim",)'))
    drain
    # ibus-daemon clears the preedit while it switches engines; that
    # isn't what the test is about.
    @events.clear
  end

  def stop
    @context = nil
    @panel = nil
    if @connection
      @connection.close_sync(nil)
      @connection = nil
    end
    if @daemon
      begin
        Process.kill(:TERM, @daemon)
      rescue Errno::ESRCH
      end
      Process.waitpid(@daemon)
      @daemon = nil
    end
    if @helper
      @helper.close
      @helper = nil
    end
    FileUtils.rm_rf(@dir) if @dir
  end

  def focus_out
    @context.focus_out
    sync
  end

  def focus_in
    @context.focus_in
    sync
  end

  # What the focused field takes, an IBus::InputPurpose.
  def set_content_type(purpose)
    @context.set_content_type(purpose.to_i, 0)
    sync_engine
  end

  # Returns whether the engine took the key. What the engine sent back
  # in the meantime goes to #events.
  def key(keyval, keycode, state=0)
    processed = @context.process_key_event(keyval, keycode, state)
    drain
    processed
  end

  # A press and a release. Returns whether the engine took each.
  def type(keyval, keycode, state=0)
    [key(keyval, keycode, state), key(keyval, keycode, state | RELEASE_MASK)]
  end

  def control(keyval, keycode)
    key(KEYVAL_CONTROL_L, KEYCODE_LEFTCTRL)
    type(keyval, keycode, CONTROL_MASK)
    key(KEYVAL_CONTROL_L, KEYCODE_LEFTCTRL, CONTROL_MASK | RELEASE_MASK)
  end

  # Waits for what the engine does in its own time, like answering
  # uim-helper-server.
  def wait_until
    deadline = Time.now + TIMEOUT
    loop do
      drain
      @helper.receive
      break if yield
      break if Time.now > deadline
      sleep(0.01)
    end
  end

  # Waits until the engine has done what it was asked before: a key
  # goes through the engine, and it answers only after that. The key
  # is the release of a key nobody pressed, which no input method
  # minds.
  def sync_engine
    key(KEYVAL_SHIFT_L, KEYCODE_LEFTSHIFT, RELEASE_MASK)
    @helper.receive
  end

  # The symbol of the input mode, which GNOME Shell shows in the top
  # bar. It skips hidden properties.
  def input_mode
    prop = find_property(@properties, "InputMode")
    return nil if prop.nil? or not prop.visible
    prop.symbol
  end

  # The labels of the menus, as GNOME Shell shows them in the input
  # source menu.
  def menu_labels
    (@properties || []).select(&:visible).collect(&:label)
  end

  # Whether the radio item for the action is the chosen one.
  def checked?(key)
    prop = find_property(@properties, key)
    not prop.nil? and prop.state == IBus::PropState::CHECKED
  end

  # As GNOME Shell does when a radio item in the menu is chosen.
  def activate_property(key)
    @panel.property_activate(key, IBus::PropState::CHECKED)
  end

  # The first lines of the messages to uim-helper-server. The same one
  # in a row counts once: some input methods tell about their
  # properties themselves when they get the focus, and the engine does
  # it again for those that don't.
  def helper_commands
    commands = @helper.messages.collect {|message| message.lines.first.chomp}
    commands.chunk_while {|a, b| a == b}.collect(&:first)
  end

  private
  def find_property(props, key)
    (props || []).each do |prop|
      return prop if prop.key == key
      found = find_property(prop.sub_props, key)
      return found if found
    end
    nil
  end

  def copy_property(prop)
    Property.new(prop.key, prop.prop_type, prop.label.text, prop.symbol.text,
                 prop.state, prop.visible?, copy_properties(prop.sub_props))
  end

  def copy_properties(props)
    copies = []
    i = 0
    while (prop = props.get(i))
      copies << copy_property(prop)
      i += 1
    end
    copies
  end

  # As GNOME Shell does: the property of the same key and type, but not
  # what is in its menu.
  def update_property(props, update)
    props.each do |prop|
      if prop.key == update.key and prop.type == update.type
        prop.label = update.label
        prop.symbol = update.symbol
        prop.state = update.state
        prop.visible = update.visible
        return true
      end
      return true if update_property(prop.sub_props, update)
    end
    false
  end

  # The properties go to the panel. GNOME Shell takes the first ones
  # after the engine changes and only updates to them after that, so
  # this does too.
  def start_panel
    @connection.call_sync("org.freedesktop.DBus", "/org/freedesktop/DBus",
                          "org.freedesktop.DBus", "RequestName",
                          GLib::Variant.parse("(\"#{PANEL_SERVICE}\", uint32 0)"),
                          nil, :none, TIMEOUT * 1000, nil)
    @panel = IBus::PanelService.new(@connection)
    @panel.signal_connect("register-properties") do |_, props|
      @properties ||= copy_properties(props) if props.get(0)
    end
    @panel.signal_connect("update-property") do |_, prop|
      update_property(@properties, copy_property(prop)) if @properties
    end
  end

  def call(path, interface, method, parameters=nil)
    @connection.call_sync(SERVICE, path, interface, method, parameters,
                          nil, :none, TIMEOUT * 1000, nil)
  end

  # Focus changes don't wait for a reply, so ask for something that
  # does: once it is back, ibus-daemon has done what came before.
  def sync
    call(PATH, "org.freedesktop.DBus.Peer", "Ping")
    drain
  end

  # Signals that came in while we waited for a reply are dispatched
  # here.
  def drain
    context = GLib::MainContext.default
    while context.iteration(false)
    end
  end

  def connect_signals
    @context.signal_connect("commit-text") do |_, text|
      @events << "commit #{text.text}"
    end
    @context.signal_connect("update-preedit-text") do |_, text, cursor, visible|
      @events << "preedit #{cursor} #{visible ? 1 : 0} #{text.text}"
    end
    @context.signal_connect("update-lookup-table") do |_, table, visible|
      if visible
        candidates = table.number_of_candidates.times.collect do |i|
          table.get_candidate(i).text
        end
        @events << ["lookup-table",
                    table.cursor_pos,
                    table.cursor_visible? ? 1 : 0,
                    table.page_size,
                    *candidates].join(" ")
      else
        @events << "hide-lookup-table"
      end
    end
    @context.signal_connect("hide-lookup-table") do
      @events << "hide-lookup-table"
    end
  end

  def wait_for
    deadline = Time.now + TIMEOUT
    until yield
      break if Time.now > deadline
      sleep(0.01)
    end
  end

  # uim.xml as installed, but starting the engine in the build tree.
  def component_xml(engine)
    <<-XML
<?xml version="1.0" encoding="utf-8"?>
<component>
  <name>org.freedesktop.IBus.uim</name>
  <description>uim</description>
  <exec>#{engine} --ibus</exec>
  <version>0</version>
  <engines>
    <engine>
      <name>uim</name>
      <longname>uim</longname>
      <language>other</language>
      <layout>default</layout>
    </engine>
  </engines>
</component>
    XML
  end

  def user_scm
    <<-SCM
(load "#{File.join(__dir__, "candidates.scm")}")
(define default-im-name '#{@default_im_name})
    SCM
  end
end

class TestIBusEngineUim < Test::Unit::TestCase
  include Keyboard

  # Hiragana ka, as an escape so that this file stays ASCII.
  KA = "\u304B"
  # Hiragana a.
  A = "\u3042"
  # SKK's labels for its latin mode and romaji input.
  DIRECT_INPUT = "\u76F4\u63A5\u5165\u529B"
  ROMAJI = "\u30ED\u30FC\u30DE\u5B57"

  def default_im_name
    "skk"
  end

  def setup
    @ibus = IBusSession.new(default_im_name)
    begin
      @ibus.start
      yield
    ensure
      @ibus.stop
    end
  end

  def test_unconsumed_key_goes_to_the_application
    # The input method is off, so the application gets the key.
    assert_equal([false, false], @ibus.type(KEYVAL_A, KEYCODE_A))
  end

  def test_preedit
    @ibus.control(KEYVAL_J, KEYCODE_J)
    assert_equal([true, true], @ibus.type(KEYVAL_K, KEYCODE_K))
    assert_equal("preedit 1 1 k", @ibus.events.grep(/\Apreedit /).last)
  end

  def test_commit
    @ibus.control(KEYVAL_J, KEYCODE_J)
    @ibus.type(KEYVAL_K, KEYCODE_K)
    @ibus.type(KEYVAL_A, KEYCODE_A)
    assert_equal(["commit #{KA}"], @ibus.events.grep(/\Acommit /))
  end

  # The press went elsewhere, before this engine was up, so the
  # application must get the release.
  def test_release_without_press_goes_to_the_application
    assert do
      not @ibus.key(KEYVAL_A, KEYCODE_A, RELEASE_MASK)
    end
  end

  # Clients that make up keys send keycode 0 for all of them, so it
  # says nothing about which press a release belongs to.
  def test_keycode_0_release_goes_to_the_application
    @ibus.control(KEYVAL_J, KEYCODE_J)
    # Return goes to the application while nothing is composed, and
    # "k" is typed before it is let go.
    assert do
      not @ibus.key(KEYVAL_RETURN, 0)
    end
    assert do
      @ibus.key(KEYVAL_K, 0)
    end
    assert do
      not @ibus.key(KEYVAL_RETURN, 0, RELEASE_MASK)
    end
  end

  # ibus-daemon commits the preedit into the application that loses
  # the focus, so it must not come back afterwards.
  def test_focus_out_drops_the_preedit
    @ibus.control(KEYVAL_J, KEYCODE_J)
    @ibus.type(KEYVAL_K, KEYCODE_K)
    @ibus.focus_out
    @ibus.focus_in
    @ibus.type(KEYVAL_A, KEYCODE_A)
    assert_equal(["commit k", "commit #{A}"], @ibus.events.grep(/\Acommit /))
  end

  # A field that takes no composed text gets the keys as they are.
  sub_test_case("content type") do
    def test_password_is_left_alone
      @ibus.control(KEYVAL_J, KEYCODE_J)
      @ibus.set_content_type(IBus::InputPurpose::PASSWORD)
      assert_equal([false, false], @ibus.type(KEYVAL_K, KEYCODE_K))
      assert_equal([], @ibus.events.grep(/\Apreedit /))
    end

    def test_pin_is_left_alone
      @ibus.control(KEYVAL_J, KEYCODE_J)
      @ibus.set_content_type(IBus::InputPurpose::PIN)
      assert_equal([false, false], @ibus.type(KEYVAL_K, KEYCODE_K))
    end

    def test_digits_is_left_alone
      @ibus.control(KEYVAL_J, KEYCODE_J)
      @ibus.set_content_type(IBus::InputPurpose::DIGITS)
      assert_equal([false, false], @ibus.type(KEYVAL_K, KEYCODE_K))
    end

    def test_number_is_left_alone
      @ibus.control(KEYVAL_J, KEYCODE_J)
      @ibus.set_content_type(IBus::InputPurpose::NUMBER)
      assert_equal([false, false], @ibus.type(KEYVAL_K, KEYCODE_K))
    end

    def test_phone_is_left_alone
      @ibus.control(KEYVAL_J, KEYCODE_J)
      @ibus.set_content_type(IBus::InputPurpose::PHONE)
      assert_equal([false, false], @ibus.type(KEYVAL_K, KEYCODE_K))
    end

    def test_free_form_still_composes
      @ibus.set_content_type(IBus::InputPurpose::FREE_FORM)
      @ibus.control(KEYVAL_J, KEYCODE_J)
      @ibus.type(KEYVAL_K, KEYCODE_K)
      assert_equal("preedit 1 1 k", @ibus.events.grep(/\Apreedit /).last)
    end

    def test_preedit_is_dropped_when_field_turns_password
      @ibus.control(KEYVAL_J, KEYCODE_J)
      @ibus.type(KEYVAL_K, KEYCODE_K)
      @ibus.set_content_type(IBus::InputPurpose::PASSWORD)
      assert_equal([false, false], @ibus.type(KEYVAL_L, KEYCODE_L))
      assert_equal(["preedit 1 1 k", "preedit 0 0 "],
                   @ibus.events.grep(/\Apreedit /))
    end

    # The press went to the application, so its release does too.
    def test_key_held_while_field_turns_free_form
      @ibus.control(KEYVAL_J, KEYCODE_J)
      @ibus.set_content_type(IBus::InputPurpose::PASSWORD)
      assert do
        not @ibus.key(KEYVAL_K, KEYCODE_K)
      end
      @ibus.set_content_type(IBus::InputPurpose::FREE_FORM)
      assert do
        not @ibus.key(KEYVAL_K, KEYCODE_K, RELEASE_MASK)
      end
    end

    # The press went to the input method, so the application never
    # sees its release.
    def test_key_held_while_field_turns_password
      @ibus.control(KEYVAL_J, KEYCODE_J)
      assert do
        @ibus.key(KEYVAL_K, KEYCODE_K)
      end
      @ibus.set_content_type(IBus::InputPurpose::PASSWORD)
      assert do
        @ibus.key(KEYVAL_K, KEYCODE_K, RELEASE_MASK)
      end
    end
  end

  sub_test_case("properties") do
    setup do
      @ibus.wait_until {@ibus.input_mode}
    end

    def test_input_mode
      assert_equal("a", @ibus.input_mode)
    end

    # uim tells no name for the other widgets.
    def test_menu_labels
      assert_equal(["Input method (SKK)",
                    "Input mode (#{DIRECT_INPUT})",
                    ROMAJI],
                   @ibus.menu_labels)
    end

    def test_input_mode_follows_the_input_method
      @ibus.control(KEYVAL_J, KEYCODE_J)
      @ibus.wait_until {@ibus.input_mode == A}
      assert_equal(A, @ibus.input_mode)
    end

    # GNOME Shell keeps the menus of the first input method.
    def test_switch_input_method
      @ibus.activate_property("action_imsw_direct")
      @ibus.wait_until {@ibus.input_mode == "-"}
      assert_equal(["Input method (Direct)", "-"],
                   [@ibus.menu_labels.first, @ibus.input_mode])
    end

    # ibus-daemon aborts when a menu and an item in it share a key.
    def test_activate_other_widget
      @ibus.activate_property("action_skk_azik")
      @ibus.wait_until {@ibus.checked?("action_skk_azik")}
      assert_equal([true, false],
                   [@ibus.checked?("action_skk_azik"),
                    @ibus.checked?("action_skk_roma")])
    end

    def test_activate
      @ibus.activate_property("action_skk_hiragana")
      @ibus.wait_until {@ibus.input_mode == A}
      @ibus.type(KEYVAL_K, KEYCODE_K)
      assert_equal([A, "preedit 1 1 k"],
                   [@ibus.input_mode, @ibus.events.grep(/\Apreedit /).last])
    end
  end

  sub_test_case("uim-helper-server") do
    setup do
      # The engine connects when it gets the focus, and tells the
      # toolbar about its input method.
      @ibus.wait_until {@ibus.helper_commands.include?("prop_list_update")}
      @ibus.sync_engine
    end

    def prop_list_updates
      @ibus.helper.messages.select do |message|
        message.start_with?("prop_list_update\n")
      end
    end

    def test_focus_in
      assert_equal(["focus_in", "prop_list_update"], @ibus.helper_commands)
    end

    # ibus-daemon hands the global engine to a context of its own
    # while the focus is away, which is no application to talk for.
    def test_focus_out
      @ibus.focus_out
      @ibus.focus_in
      @ibus.wait_until {@ibus.helper_commands.size >= 5}
      @ibus.sync_engine
      assert_equal(["focus_in", "prop_list_update",
                    "focus_out", "focus_in", "prop_list_update"],
                   @ibus.helper_commands)
    end

    # Another uim client, say a GTK application with GTK_IM_MODULE=uim,
    # got the focus: the toolbar is theirs until the engine gets the
    # focus back.
    def test_focus_in_elsewhere
      n_updates = prop_list_updates.size
      @ibus.helper.send_message("focus_in\n")
      @ibus.helper.send_message("commit_string\nabc\n")
      # Every engine takes this one, so the engine has seen the others
      # once it has.
      @ibus.helper.send_message("im_change_whole_desktop\ncandidates\n")
      @ibus.wait_until do
        @ibus.type(KEYVAL_A, KEYCODE_A)
        not @ibus.events.grep(/\Alookup-table /).empty?
      end
      assert_equal([[], n_updates],
                   [@ibus.events.grep(/\Acommit /), prop_list_updates.size])

      @ibus.focus_out
      @ibus.focus_in
      # The engine must have the focus before the text comes.
      @ibus.sync_engine
      @ibus.helper.send_message("commit_string\ndef\n")
      @ibus.wait_until {not @ibus.events.grep(/\Acommit /).empty?}
      assert_equal(["commit def"], @ibus.events.grep(/\Acommit /))
    end

    def test_im_list_get
      @ibus.helper.send_message("im_list_get\n")
      @ibus.wait_until {@ibus.helper_commands.include?("im_list")}
      im_list = @ibus.helper.messages.find do |message|
        message.start_with?("im_list\n")
      end
      assert_equal("skk\tJapanese\tuim version of SKK input method\tselected\n",
                   im_list.lines.grep(/\Askk\t/).first)
    end

    def test_prop_activate
      n_updates = prop_list_updates.size
      @ibus.helper.send_message("prop_activate\naction_skk_hiragana\n")
      @ibus.wait_until {prop_list_updates.size > n_updates}
      @ibus.type(KEYVAL_K, KEYCODE_K)
      assert_equal("preedit 1 1 k", @ibus.events.grep(/\Apreedit /).last)
    end

    def test_im_change_this_text_area_only
      n_updates = prop_list_updates.size
      @ibus.helper.send_message("im_change_this_text_area_only\ncandidates\n")
      @ibus.wait_until {prop_list_updates.size > n_updates}
      @ibus.type(KEYVAL_A, KEYCODE_A)
      assert_equal(1, @ibus.events.grep(/\Alookup-table /).size)
    end

    def test_commit_string
      @ibus.helper.send_message("commit_string\nabc\n")
      @ibus.wait_until {not @ibus.events.empty?}
      assert_equal(["commit abc"], @ibus.events)
    end

    def test_commit_string_with_charset
      @ibus.helper.send_message("commit_string\ncharset=EUC-JP\n\xA4\xA2\n".b)
      @ibus.wait_until {not @ibus.events.empty?}
      assert_equal(["commit #{A}"], @ibus.events)
    end

    # The charset line isn't the text.
    def test_commit_string_with_charset_and_no_text
      @ibus.helper.send_message("commit_string\ncharset=UTF-8\n")
      @ibus.helper.send_message("commit_string\nabc\n")
      @ibus.wait_until {not @ibus.events.empty?}
      assert_equal(["commit abc"], @ibus.events)
    end

    def test_reconnect
      @ibus.helper.disconnect
      @ibus.helper.messages.clear
      # The engine must notice before the focus moves.
      @ibus.sync_engine
      @ibus.focus_out
      @ibus.focus_in
      @ibus.wait_until {@ibus.helper_commands.include?("prop_list_update")}
      assert_equal(["focus_in", "prop_list_update"], @ibus.helper_commands)
    end
  end

  sub_test_case("candidates") do
    def default_im_name
      "candidates"
    end

    def candidates
      (0...12).collect {|i| "candidate#{i}"}.join(" ")
    end

    def test_activate
      @ibus.type(KEYVAL_A, KEYCODE_A)
      assert_equal("lookup-table 0 0 5 #{candidates}",
                   @ibus.events.grep(/\Alookup-table /).last)
    end

    def test_select
      @ibus.type(KEYVAL_A, KEYCODE_A)
      @ibus.type(KEYVAL_B, KEYCODE_B)
      assert_equal("lookup-table 7 1 5 #{candidates}",
                   @ibus.events.grep(/\Alookup-table /).last)
    end

    def test_shift_page_moves_the_selection
      @ibus.type(KEYVAL_A, KEYCODE_A)
      @ibus.type(KEYVAL_B, KEYCODE_B)
      @ibus.type(KEYVAL_N, KEYCODE_N)
      # Candidate 7 is the third on the second page, and the third on
      # the last page is past the end, so the last one is selected.
      assert_equal(["commit [11]",
                    "lookup-table 11 1 5 #{candidates}"],
                   @ibus.events.grep(/\A(?:commit|lookup-table) /).last(2))
    end

    def test_deactivate
      @ibus.type(KEYVAL_A, KEYCODE_A)
      @ibus.type(KEYVAL_X, KEYCODE_X)
      assert_equal("hide-lookup-table", @ibus.events.last)
    end
  end
end
