// swiftc pboard-types.swift
import AppKit
import Foundation

let pb = NSPasteboard.general
for item in pb.pasteboardItems ?? [] {
    for type in item.types {
        print(type.rawValue)
    }
}
