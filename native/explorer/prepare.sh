#!/usr/bin/env bash
set -euo pipefail

project_dir="$(cd -- "$(dirname -- "${BASH_SOURCE[0]}")" && pwd)"
cache_dir="$project_dir/../../.cache/lynx-explorer"
archive="$cache_dir/source.tar.gz"
source_dir="$cache_dir/lynx-4.0.3"
target_dir="$project_dir/build/upstream"
apk="$cache_dir/upstream.apk"

mkdir -p "$cache_dir"
if [[ ! -f "$archive" ]]; then
  curl --fail --location --output "$archive.download" 'https://github.com/lynx-family/lynx/archive/refs/tags/4.0.3.tar.gz'
  mv "$archive.download" "$archive"
fi
printf '%s  %s\n' 'e699857ed30aa167957f7ae8375da2b87ac7210568283a206dbd611cd7075098' "$archive" | sha256sum --check --status
if [[ ! -d "$source_dir" ]]; then
  tar --extract --gzip --file "$archive" --directory "$cache_dir" --no-same-owner
fi

mkdir -p "$target_dir"
cp -R "$source_dir/explorer/android/lynx_explorer/src/main/." "$target_dir/"
sed -i \
  -e 's/    package="com.lynx.explorer"//g' \
  -e 's/android:allowBackup="true"/android:allowBackup="false"/g' \
  -e 's/android:label="@string\/app_name"/android:label="MeroChat Explorer"/g' \
  "$target_dir/AndroidManifest.xml"
sed -z -i 's@      <activity\n        android:name="com\.tiktok\.sparkling\.[[:print:][:space:]]*    </application>@    </application>@' "$target_dir/AndroidManifest.xml"
sed -i \
  -e 's/import com.lynx.service.http.LynxHttpService;/import com.merochat.explorer.SessionHttpService;\nimport com.merochat.explorer.SessionModule;\nimport com.merochat.explorer.ThemeModule;\nimport com.merochat.explorer.RoomModule;\nimport com.merochat.explorer.ImageModule;\nimport com.merochat.explorer.DatePickerModule;\nimport com.merochat.explorer.SessionWebSocketModule;/g' \
  -e 's/LynxHttpService.INSTANCE/new SessionHttpService(this)/g' \
  -e 's/LynxModuleAdapter.getInstance().Init(this);/LynxModuleAdapter.getInstance().Init(this);\n    LynxEnv.inst().registerModule("MeroChatSession", SessionModule.class);\n    LynxEnv.inst().registerModule("MeroChatTheme", ThemeModule.class);\n    LynxEnv.inst().registerModule("MeroChatRoom", RoomModule.class);\n    LynxEnv.inst().registerModule("MeroChatImage", ImageModule.class);\n    LynxEnv.inst().registerModule("MeroChatDatePicker", DatePickerModule.class);\n    LynxEnv.inst().registerModule("MeroChatWebSocket", SessionWebSocketModule.class);/g' \
  "$target_dir/java/com/lynx/explorer/ExplorerApplication.java"

if [[ ! -f "$apk" ]]; then
  curl --fail --location --output "$apk.download" 'https://github.com/lynx-family/lynx/releases/download/4.0.3/LynxExplorer-noasan-release.apk'
  mv "$apk.download" "$apk"
fi
printf '%s  %s\n' '6d8fae61cf4da79fded54b26d7bc947af09173669e957bad14809fcc12056e2a' "$apk" | sha256sum --check --status
unzip -oq "$apk" 'assets/*' -d "$target_dir"
printf '%s\n' 'Prepared Explorer 4.0.3'
