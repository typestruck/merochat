#!/usr/bin/env python3
"""Prepare the pinned upstream Explorer sources; custom code stays in src/main."""
from pathlib import Path
import hashlib
import shutil
import tarfile
import urllib.request
import zipfile

project = Path(__file__).resolve().parent
cache = project.parents[1] / '.cache/lynx-explorer'
cache.mkdir(parents=True, exist_ok=True)
archive = cache / 'source.tar.gz'
if not archive.exists():
    urllib.request.urlretrieve('https://github.com/lynx-family/lynx/archive/refs/tags/4.0.3.tar.gz', archive)
assert hashlib.sha256(archive.read_bytes()).hexdigest() == 'e699857ed30aa167957f7ae8375da2b87ac7210568283a206dbd611cd7075098', 'Unexpected Explorer source archive'
source = cache / 'lynx-4.0.3'
if not source.exists():
    with tarfile.open(archive) as contents:
        contents.extractall(cache, filter='data')
target = project / 'build/upstream'
shutil.copytree(source / 'explorer/android/lynx_explorer/src/main', target, dirs_exist_ok=True)
p = target / 'AndroidManifest.xml'
s = p.read_text().replace('    package="com.lynx.explorer"', '').replace('android:allowBackup="true"', 'android:allowBackup="false"').replace('android:label="@string/app_name"', 'android:label="MeroChat Explorer"')
start = s.index('      <activity\n        android:name="com.tiktok.sparkling.')
s = s[:start] + s[s.index('    </application>', start):]
p.write_text(s)
p = target / 'java/com/lynx/explorer/ExplorerApplication.java'
s = p.read_text().replace('import com.lynx.service.http.LynxHttpService;', 'import com.merochat.explorer.SessionHttpService;\nimport com.merochat.explorer.SessionModule;')
s = s.replace('LynxHttpService.INSTANCE', 'new SessionHttpService(this)')
s = s.replace('LynxModuleAdapter.getInstance().Init(this);', 'LynxModuleAdapter.getInstance().Init(this);\n    LynxEnv.inst().registerModule("MeroChatSession", SessionModule.class);')
p.write_text(s)
print('Prepared Explorer 4.0.3')

# Keep Explorer's published homepage/QR UI without rebuilding its frontend.
apk = cache / 'upstream.apk'
if not apk.exists():
    urllib.request.urlretrieve('https://github.com/lynx-family/lynx/releases/download/4.0.3/LynxExplorer-noasan-release.apk', apk)
assert hashlib.sha256(apk.read_bytes()).hexdigest() == '6d8fae61cf4da79fded54b26d7bc947af09173669e957bad14809fcc12056e2a', 'Unexpected Explorer APK'
with zipfile.ZipFile(apk) as package:
    for name in package.namelist():
        if name.startswith('assets/') and not name.endswith('/'):
            destination = target / name
            destination.parent.mkdir(parents=True, exist_ok=True)
            destination.write_bytes(package.read(name))
