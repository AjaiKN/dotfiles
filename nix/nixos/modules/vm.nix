{
  config,
  lib,
  pkgs,
  ...
}:

{
  environment.systemPackages = with pkgs; [
    # for VM
    spice-vdagent
    spice
  ];

  # https://discourse.nixos.org/t/nixos-as-a-guest-os-in-qemu-kvm-how-to-share-clipboard-displaying-scaling-etc/8124
  services.xserver.videoDrivers = [ "qxl" ];
  services.dbus.packages = [ pkgs.gnome2.GConf ];
  services.qemuGuest.enable = true;
  services.spice-vdagentd.enable = true;
  services.spice-webdavd.enable = true;
  systemd.user.services.spice-vdagent-client = {
    description = "spice-vdagent client";
    wantedBy = [ "graphical-session.target" ];
    serviceConfig = {
      ExecStart = "${pkgs.spice-vdagent}/bin/spice-vdagent -x";
      Restart = "on-failure";
      RestartSec = "5";
    };
  };
  systemd.user.services.spice-vdagent-client.enable = true;
}
