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

# These tests run uim-sni on a D-Bus session bus of their own. They
# stand in for uim-helper-server and for a panel such as Waybar that
# provides org.kde.StatusNotifierWatcher and shows the items.

require "dbus"
require "fileutils"
require "socket"
require "tmpdir"

class TestUimSni < Test::Unit::TestCase
  ITEM_INTERFACE = "org.kde.StatusNotifierItem"
  MENU_INTERFACE = "com.canonical.dbusmenu"
  WATCHER_NAME = "org.kde.StatusNotifierWatcher"
  TIMEOUT = 5

  class Watcher < DBus::Object
    attr_reader :services
    attr_writer :failing

    def initialize
      super("/StatusNotifierWatcher")
      @services = []
    end

    dbus_interface "org.kde.StatusNotifierWatcher" do
      dbus_method :RegisterStatusNotifierItem, "in service:s" do |service|
        @services << service
        raise DBus.error, "failing" if @failing
        nil
      end
    end
  end

  def setup
    Dir.mktmpdir("uim-sni") do |dir|
      @dir = dir
      @runtime_dir = File.join(@dir, "runtime")
      socket_dir = File.join(@runtime_dir, "uim", "socket")
      FileUtils.mkdir_p(socket_dir, mode: 0700)
      UNIXServer.open(File.join(socket_dir, "uim-helper")) do |helper_server|
        @helper_server = helper_server
        start_bus do
          start_host do
            yield
          end
        end
      end
    end
  end

  def start_bus
    command_line = [
      ENV.fetch("DBUS_DAEMON", "dbus-daemon"),
      "--session",
      "--nofork",
      "--print-address",
      "--address=unix:path=#{File.join(@dir, "bus")}",
    ]
    IO.popen(command_line) do |output|
      @bus_address = output.gets.chomp
      begin
        yield
      ensure
        Process.kill(:TERM, output.pid)
      end
    end
  end

  # Looks at the items as a panel does while the block runs.
  def start_host
    @host_bus = DBus::BusConnection.new(@bus_address)
    begin
      @signals = []
      rule = DBus::MatchRule.new
      rule.type = "signal"
      rule.interface = ITEM_INTERFACE
      @host_bus.add_match(rule) do |message|
        @signals << [message.sender, message.member]
      end
      yield
    ensure
      @host_bus.message_queue.socket.close
    end
  end

  # Stands in for a panel while the block runs.
  def start_watcher
    @watcher_bus = DBus::BusConnection.new(@bus_address)
    begin
      @watcher = Watcher.new
      @watcher_bus.object_server.export(@watcher)
      @watcher_bus.request_name(WATCHER_NAME)
      yield
    ensure
      @watcher_bus.message_queue.socket.close
      @watcher_bus = nil
    end
    # The next watcher can take the name.
    wait_until {not has_owner?(WATCHER_NAME)}
  end

  def spawn_uim_sni(**options)
    env = {
      "DBUS_SESSION_BUS_ADDRESS" => @bus_address,
      "HOME" => @dir,
      "LC_ALL" => "C",
      "XDG_RUNTIME_DIR" => @runtime_dir,
    }
    spawn(env, ENV.fetch("UIM_SNI"), **options)
  end

  def start_uim_sni
    pid = spawn_uim_sni
    begin
      unless IO.select([@helper_server], nil, nil, TIMEOUT)
        raise "uim-sni didn't connect to uim-helper-server"
      end
      @helper = @helper_server.accept
      begin
        yield
      ensure
        @helper.close
      end
    ensure
      Process.kill(:TERM, pid)
      Process.waitpid(pid)
    end
  end

  def read_helper_message
    message = +""
    until message.end_with?("\n\n")
      unless IO.select([@helper], nil, nil, TIMEOUT)
        raise "no message from uim-sni: #{message.inspect}"
      end
      message << @helper.readpartial(4096)
    end
    message.chomp("\n")
  end

  # branches: [[indication_id, iconic_label, label_string, leaves], ...]
  # leaves (optional): [[indication_id, iconic_label, label_string,
  #                      short_desc, action_id, activity], ...]
  def send_prop_list(branches, charset: "UTF-8")
    message = "prop_list_update\ncharset=#{charset}\n".b
    branches.each do |indication_id, iconic_label, label_string, leaves = []|
      branch = [indication_id, iconic_label, label_string]
      message << "branch\t#{branch.join("\t")}\n".encode(charset).b
      leaves.each do |leaf|
        message << "leaf\t#{leaf.join("\t")}\n".encode(charset).b
      end
    end
    @helper.write(message + "\n")
  end

  def send_prop_update_custom(symbol, value)
    @helper.write("prop_update_custom\n#{symbol}\n#{value}\n\n")
  end

  def process_buffered_messages(bus)
    while (message = bus.message_queue.message_from_buffer_nonblock)
      bus.process(message)
    end
  end

  def pump(timeout)
    buses = [@watcher_bus, @host_bus].compact
    # A synchronous call may have read messages after its reply into
    # the buffer. IO.select() doesn't see them.
    buses.each do |bus|
      process_buffered_messages(bus)
    end
    sockets = buses.collect {|bus| bus.message_queue.socket}
    ready, = IO.select(sockets, nil, nil, timeout)
    return unless ready
    ready.each do |socket|
      bus = buses.find {|b| b.message_queue.socket == socket}
      bus.message_queue.buffer_from_socket_nonblock
      process_buffered_messages(bus)
    end
  end

  # Processes the messages that come in the duration.
  def drain(duration)
    deadline = Time.now + duration
    pump(deadline - Time.now) while Time.now < deadline
  end

  def wait_until
    deadline = Time.now + TIMEOUT
    until yield
      raise "timed out" if Time.now > deadline
      pump(0.1)
    end
  end

  def wait_registered(n)
    wait_until {@watcher.services.size >= n}
    @watcher.services
  end

  def item(service)
    object = @host_bus.service(service).object("/StatusNotifierItem")
    object.introspect
    object[ITEM_INTERFACE]
  end

  def menu(service)
    object = @host_bus.service(service).object("/MenuBar")
    object.introspect
    object[MENU_INTERFACE]
  end

  def titles(services)
    services.collect {|service| item(service)["Title"]}
  end

  def has_owner?(service)
    @host_bus.proxy.NameHasOwner(service).first
  end

  BRANCHES = [
    ["anthy", "An", "Anthy"],
    ["ja_hiragana", "あ", "Hiragana"],
    ["ja_romaji", "R", "Romaji"],
  ]

  sub_test_case("watcher: running") do
    def setup
      super do
        start_watcher do
          start_uim_sni do
            yield
          end
        end
      end
    end

    test("prop_list_get") do
      assert_equal("prop_list_get\n", read_helper_message)
    end

    test("order") do
      send_prop_list(BRANCHES)
      assert_equal(["Anthy", "Hiragana", "Romaji"],
                   titles(wait_registered(3)))
    end

    test("IconName") do
      send_prop_list(BRANCHES)
      assert_equal("ja_hiragana", item(wait_registered(3)[1])["IconName"])
    end

    test("IconThemePath") do
      send_prop_list(BRANCHES)
      assert_equal(ENV.fetch("UIM_SNI_PIXMAPSDIR"),
                   item(wait_registered(3)[1])["IconThemePath"])
    end

    test("IconPixmap") do
      send_prop_list(BRANCHES)
      assert_equal([], item(wait_registered(3)[1])["IconPixmap"])
    end

    test("AttentionIconName") do
      send_prop_list(BRANCHES)
      assert_equal("", item(wait_registered(3)[1])["AttentionIconName"])
    end

    test("AttentionIconPixmap") do
      send_prop_list(BRANCHES)
      assert_equal([], item(wait_registered(3)[1])["AttentionIconPixmap"])
    end

    test("ToolTip") do
      send_prop_list(BRANCHES)
      assert_equal(["ja_hiragana", [], "Hiragana", ""],
                   item(wait_registered(3)[1])["ToolTip"])
    end

    test("charset") do
      send_prop_list([["ja_hiragana", "あ", "ひらがな"]], charset: "EUC-JP")
      assert_equal(["ひらがな"], titles(wait_registered(1)))
    end

    test("fewer branches") do
      send_prop_list(BRANCHES)
      services = wait_registered(3)
      send_prop_list(BRANCHES[0, 2])
      wait_until {not has_owner?(services[2])}
      assert_equal([true, true],
                   services[0, 2].collect {|service| has_owner?(service)})
    end

    test("more branches") do
      send_prop_list(BRANCHES[0, 2])
      wait_registered(2)
      send_prop_list(BRANCHES)
      assert_equal(["Anthy", "Hiragana", "Romaji"],
                   titles(wait_registered(3)))
    end

    test("new icon") do
      send_prop_list(BRANCHES)
      services = wait_registered(3)
      drain(0.5)
      @signals.clear
      send_prop_list([
                       BRANCHES[0],
                       ["ja_katakana", "ア", "Hiragana"],
                       BRANCHES[2],
                     ])
      drain(0.5)
      assert_equal([[services[1], "NewIcon"], [services[1], "NewToolTip"]],
                   @signals)
    end

    sub_test_case("menu") do
      def setup
        super do
          read_helper_message # prop_list_get
          send_prop_list([
                           [
                             "ja_hiragana",
                             "あ",
                             "Hiragana",
                             [
                               [
                                 "ja_hiragana",
                                 "あ",
                                 "Hiragana",
                                 "Hiragana mode",
                                 "action_hiragana",
                                 "*",
                               ],
                               [
                                 "separator",
                                 "",
                                 "",
                                 "",
                                 "",
                                 "",
                               ],
                               [
                                 "ja_halfkana",
                                 "ｱ",
                                 "Half_kana",
                                 "Half-width katakana mode",
                                 "action_halfkana",
                                 "",
                               ],
                             ],
                           ],
                         ])
          @service = wait_registered(1)[0]
          yield
        end
      end

      test("Menu") do
        assert_equal("/MenuBar", item(@service)["Menu"])
      end

      test("ItemIsMenu") do
        assert_true(item(@service)["ItemIsMenu"])
      end

      test("layout") do
        assert_equal([
                       1,
                       [
                         0,
                         {"children-display" => "submenu"},
                         [
                           [
                             1,
                             {
                               "label" => "Hiragana",
                               "toggle-type" => "radio",
                               "toggle-state" => 1,
                             },
                             [],
                           ],
                           [2, {"type" => "separator"}, []],
                           [
                             3,
                             {
                               "label" => "Half__kana",
                               "toggle-type" => "radio",
                               "toggle-state" => 0,
                             },
                             [],
                           ],
                         ],
                       ],
                     ],
                     menu(@service).GetLayout(0, -1, []))
      end

      test("GetGroupProperties") do
        assert_equal([[[3, {
                              "label" => "Half__kana",
                              "toggle-type" => "radio",
                              "toggle-state" => 0,
                            }]]],
                     menu(@service).GetGroupProperties([3, 4], []))
      end

      test("GetProperty") do
        assert_equal(["Hiragana"], menu(@service).GetProperty(1, "label"))
      end

      test("clicked") do
        menu(@service).Event(3, "clicked", DBus.variant("i", 0), 0)
        assert_equal("prop_activate\naction_halfkana\n",
                     read_helper_message)
      end

      test("GetLayout of a leaf") do
        assert_equal([1, [2, {"type" => "separator"}, []]],
                     menu(@service).GetLayout(2, -1, []))
      end

      test("GetLayout of an unknown item") do
        assert_raise(DBus::Error) do
          menu(@service).GetLayout(4, -1, [])
        end
      end

      # The IDs change with the leaves so that an event for the menu
      # shown before doesn't choose another leaf.
      test("event for an old menu") do
        send_prop_list([
                         [
                           "ja_katakana",
                           "ア",
                           "Katakana",
                           [
                             [
                               "ja_katakana",
                               "ア",
                               "Katakana",
                               "Katakana mode",
                               "action_katakana",
                               "*",
                             ],
                             [
                               "ja_halfkana",
                               "ｱ",
                               "Half_kana",
                               "Half-width katakana mode",
                               "action_halfkana",
                               "",
                             ],
                           ],
                         ],
                       ])
        wait_until {menu(@service).GetLayout(0, -1, [])[0] == 2}
        # 1 is "Hiragana" of the old menu.
        menu(@service).Event(1, "clicked", DBus.variant("i", 0), 0)
        # 5 is "Half_kana" of the new menu.
        menu(@service).Event(5, "clicked", DBus.variant("i", 0), 0)
        assert_equal("prop_activate\naction_halfkana\n",
                     read_helper_message)
      end

      test("revision") do
        send_prop_list([["ja_hiragana", "あ", "Hiragana", []]])
        wait_until {menu(@service).GetLayout(0, -1, [])[0] == 2}
        assert_equal([], menu(@service).GetLayout(0, -1, [])[1][2])
      end
    end

    sub_test_case("dark background") do
      def setup
        super do
          send_prop_list(BRANCHES)
          @services = wait_registered(3)
          send_prop_update_custom("toolbar-icon-for-dark-background?", "#t")
          drain(0.5)
          yield
        end
      end

      test("IconName") do
        assert_equal("ja_hiragana_dark_background",
                     item(@services[1])["IconName"])
      end

      # anthy has no icon for a dark background.
      test("no icon") do
        assert_equal("anthy", item(@services[0])["IconName"])
      end

      test("off") do
        send_prop_update_custom("toolbar-icon-for-dark-background?", "#f")
        drain(0.5)
        assert_equal("ja_hiragana", item(@services[1])["IconName"])
      end
    end

    test("failed registration") do
      @watcher.failing = true
      send_prop_list(BRANCHES)
      wait_registered(3)
      drain(0.5)
      assert_equal(3, @watcher.services.size)
    end

    test("second instance") do
      pid = spawn_uim_sni(err: File::NULL)
      status = nil
      wait_until {status = Process.waitpid2(pid, Process::WNOHANG)&.last}
      assert_false(status.success?)
    end
  end

  sub_test_case("watcher: started later") do
    def setup
      super do
        start_uim_sni do
          yield
        end
      end
    end

    test("order") do
      send_prop_list(BRANCHES)
      start_watcher do
        assert_equal(["Anthy", "Hiragana", "Romaji"],
                     titles(wait_registered(3)))
      end
    end

    test("restarted") do
      send_prop_list(BRANCHES)
      start_watcher do
        wait_registered(3)
      end
      start_watcher do
        assert_equal(["Anthy", "Hiragana", "Romaji"],
                     titles(wait_registered(3)))
      end
    end
  end
end
