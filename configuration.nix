# Edit this configuration file to define what should be installed on
# your system.  Help is available in the configuration.nix(5) man page
# and in the NixOS manual (accessible by running ‘nixos-help’).

{ config, pkgs, ... }:

{
  imports = [ # Include the results of the hardware scan.
    /etc/nixos/hardware-configuration.nix
    <home-manager/nixos>
  ];

  # Use the systemd-boot EFI boot loader.
  boot.loader.systemd-boot.enable = true;
  boot.loader.systemd-boot.editor = false;
  boot.loader.timeout = 3;
  boot.loader.efi.canTouchEfiVariables = true;
  boot.initrd.luks.devices = {
    root = {
      device = "/dev/disk/by-uuid/0a624385-4c0c-4cc1-b628-dbe97689c49e";
      preLVM = true;
      allowDiscards = true;
    };
  };

  # use unfree packages
  nixpkgs.config.allowUnfree = true;

  networking.hostName = "idanpad"; # Define your hostname.
  # networking.wireless.enable = true;  # Enables wireless support via wpa_supplicant.
  networking.networkmanager.enable = true;

  # Set your time zone.
  time.timeZone = "America/New_York";

  # The global useDHCP flag is deprecated, therefore explicitly set to false here.
  # Per-interface useDHCP will be mandatory in the future, so this generated config
  # replicates the default behaviour.
  networking.useDHCP = false;
  networking.interfaces.wlp2s0.useDHCP = true;

  # Configure network proxy if necessary
  # networking.proxy.default = "http://user:password@proxy:port/";
  # networking.proxy.noProxy = "127.0.0.1,localhost,internal.domain";

  # Select internationalisation properties.
  i18n.defaultLocale = "en_US.UTF-8";
  console = {
    font = "Lat2-Terminus16";
    keyMap = "us";
  };

  # Enable CUPS to print documents.
  # services.printing.enable = true;

  # Enable sound.
  sound.enable = true;
  hardware.pulseaudio.package =
    pkgs.pulseaudioFull; # support for bluetooth headsets
  hardware.pulseaudio.enable = true;
  hardware.bluetooth.enable = true;

  # Enable KVM/QEMU
  virtualisation.libvirtd.enable = true;

  # Enable the X11 windowing system.
  services.xserver.enable = true;

  # Config autologin with xmonad
  # services.xserver.windowManager.xmonad.enable = true;
  # services.xserver.windowManager.xmonad.enableContribAndExtras = true;
  services.xserver.displayManager.autoLogin.enable = true;
  services.xserver.displayManager.autoLogin.user = "ghostpepper";
  services.xserver.displayManager.defaultSession = "xsession";
  services.xserver.displayManager.session = [{
    manage = "desktop";
    name = "xsession";
    start = "exec $HOME/.xsession";
  }];

  # Enable touchpad support (enabled default in most desktopManager).
  services.xserver.libinput.enable = true;
  services.xserver.libinput.touchpad.tapping = false;

  # Configure keymap in X11
  services.xserver.layout = "us";
  # services.xserver.xkbOptions = "eurosign:e";
  services.dbus.packages = with pkgs; [ gnome3.dconf ];

  # Allow fingerprint auth if enrolled with =fprintd-enroll <username>=
  # Unfortunately, the ideapad does not have a driver that is compatible with fprintd :(
  # services.fprintd.enable = true;
  # services.fprintd.tod.enable = true;
  # services.fprintd.tod.driver = pkgs.libfprint-2-tod1-goodix;
  # security.pam.services.login.fprintAuth = true;

  # Allow updating of firmware from D-Bus
  # You can use this with the following:
  # - fwupdmgr get-devices
  # - fwupdmgr refresh --force
  # - fwupdmgr update
  services.fwupd.enable = true;

  # Define a user account. Don't forget to set a password with ‘passwd’.
  users.users.ghostpepper = {
    isNormalUser = true;
    home = "/home/ghostpepper";
    createHome = true;
    description = "Daniel Pagan";
    shell = pkgs.zsh;
    extraGroups = [ "wheel" "networkmanager" "libvirtd" ];
  };

  home-manager.users.ghostpepper = { pkgs, ... }: {
    nixpkgs.config.allowUnfree = true;
    home.packages = with pkgs; [

      # Fonts
      (nerdfonts.override { fonts = [ "FiraCode" ]; })
      noto-fonts
      noto-fonts-emoji
      ibm-plex

      # Diagnostic Tools
      brightnessctl
      playerctl
      pamixer
      stow
      thefuck
      xclip
      udisks
      networkmanagerapplet
      lshw
      pciutils
      usbutils
      nmap
      virt-manager
      binutils
      file
      p7zip

      # Applications
      scrot
      haskellPackages.xmobar
      xsecurelock
      feh
      gimp
      pavucontrol
      google-chrome
      xournalpp
      alacritty
      spotify
      slack
      discord
      zoom-us
      signal-desktop

      # Emacs
      # deps for doom emacs doctor to not complain
      # definitely use nix-env for actualy project development
      ((emacsPackagesNgGen emacs).emacsWithPackages (epkgs: [ epkgs.vterm ]))
      texlive.combined.scheme-full
      ripgrep
      fd
      imagemagick
      nixfmt
      aspell
      aspellDicts.en
      gnumake
      cmake
      glslang
      go
      gocode
      gomodifytags
      gotests
      gore
      ghc
      hlint
      cabal-install
      jq
      mdl
      pandoc
      python39
      black
      python39Packages.pyflakes
      python39Packages.isort
      python39Packages.nose
      python39Packages.pytest
      pipenv
      rust-analyzer
      cargo
      rustc
      shellcheck
      html-tidy
      nodePackages.stylelint
      nodePackages.js-beautify
    ];

    xsession.enable = true;
    # set the background with feh here
    xsession.initExtra = "~/.fehbg &";
    xsession.profileExtra = "source ~/.secrets";
    xsession.windowManager.xmonad = {
      enable = true;
      enableContribAndExtras = true;
    };

    xdg.userDirs = {
      enable = true;
      createDirectories = true;
    };

    gtk = {
      enable = true;
      theme = {
        package = pkgs.arc-theme;
        name = "Arc-Dark";
      };
    };

    services = {
      gpg-agent = {
        enable = true;
        defaultCacheTtl = 7200;
        defaultCacheTtlSsh = 7200;
        enableSshSupport = true;
        pinentryFlavor = "gtk2";
      };
    };

    programs = {
      zsh = {
        enable = true;
        # enableSyntaxHighlighting = true;
        enableCompletion = true;
        enableAutosuggestions = true;
        shellAliases = {
          update = "sudo nixos-rebuild switch";
          upgrade = "sudo nixos-rebuild switch --upgrade";
        };
        oh-my-zsh = {
          enable = true;
          plugins = [ "git" "thefuck" ];
          theme = "robbyrussell";
        };
      };
      git = {
        enable = true;
        userName = "ghost-pep";
        userEmail = "deapagan@gmail.com";
        signing.key = "18E17B9EEE4C187B";
        signing.signByDefault = true;
      };
      gpg.enable = true;
      rofi = {
        enable = true;
        theme = "Arc-Dark";
        extraConfig = {
          modi = "drun,run,ssh";
          show-icons = true;
          sidebar-mode = true;
        };
      };
    };
  };

  # List packages installed in system profile. To search, run:
  # $ nix search wget
  environment.systemPackages = with pkgs; [
    vim # Do not forget to add an editor to edit configuration.nix! The Nano editor is also installed by default.
    wget
    curl
    tmux
  ];

  # Some programs need SUID wrappers, can be configured further or are
  # started in user sessions.
  # programs.mtr.enable = true;

  # List services that you want to enable:

  # Enable the OpenSSH daemon.
  # services.openssh.enable = true;

  # Open ports in the firewall.
  # networking.firewall.allowedTCPPorts = [ ... ];
  # networking.firewall.allowedUDPPorts = [ ... ];
  # Or disable the firewall altogether.
  # networking.firewall.enable = false;
  # networking.enableIPv6 = false;

  # This value determines the NixOS release from which the default
  # settings for stateful data, like file locations and database versions
  # on your system were taken. It‘s perfectly fine and recommended to leave
  # this value at the release version of the first install of this system.

  # (e.g. man configuration.nix or on https://nixos.org/nixos/options.html).
  system.stateVersion = "21.05"; # Did you read the comment?

}

