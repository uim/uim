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
# Run this under dbus-run-session with IBUS_ENGINE_UIM set. Put
# IBUS_UIM_DEBUG in the environment to see what the engine makes of
# the keys.

require "fileutils"
require "tmpdir"

require_relative "helper"

# X keysyms, evdev key codes and IBus modifiers.
module Keyboard
  KEYVAL_A = 0x61
  KEYVAL_B = 0x62
  KEYVAL_J = 0x6a
  KEYVAL_K = 0x6b
  KEYVAL_N = 0x6e
  KEYVAL_X = 0x78
  KEYVAL_RETURN = 0xff0d
  KEYVAL_CONTROL_L = 0xffe3
  KEYCODE_A = 30
  KEYCODE_B = 48
  KEYCODE_J = 36
  KEYCODE_K = 37
  KEYCODE_N = 49
  KEYCODE_X = 45
  KEYCODE_LEFTCTRL = 29

  CONTROL_MASK = IBus::ModifierType::CONTROL_MASK.to_i
  RELEASE_MASK = IBus::ModifierType::RELEASE_MASK.to_i
end

class IBusSession
  include Keyboard

  TIMEOUT = 10

  SERVICE = "org.freedesktop.IBus"
  PATH = "/org/freedesktop/IBus"

  # What the engine sent back, one string each:
  #
  #   commit TEXT
  #   preedit CURSOR VISIBLE TEXT
  #   lookup-table CURSOR CURSOR_VISIBLE PAGE_SIZE CANDIDATE...
  #   hide-lookup-table
  attr_reader :events

  def initialize(default_im_name)
    @default_im_name = default_im_name
    @dir = nil
    @daemon = nil
    @connection = nil
    @context = nil
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

    environment = {
      "IBUS_COMPONENT_PATH" => component_dir,
      "LIBUIM_USER_SCM_FILE" => scm_file,
      "HOME" => @dir,
      "XDG_CONFIG_HOME" => File.join(@dir, "config"),
      "XDG_CACHE_HOME" => File.join(@dir, "cache"),
      # The desktop's settings stay out of the test.
      "GSETTINGS_BACKEND" => "memory",
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

  private
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
