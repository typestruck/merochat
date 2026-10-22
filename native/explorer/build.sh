#!/usr/bin/env bash
set -euo pipefail
if [[ -z "${ANDROID_HOME:-}" && -d "$HOME/Android/Sdk" ]]; then
  export ANDROID_HOME="$HOME/Android/Sdk"
fi
project_dir="$(cd -- "$(dirname -- "${BASH_SOURCE[0]}")" && pwd)"
bash "$project_dir/prepare.sh"
java_command=java
if [[ -n "${JAVA_HOME:-}" ]]; then java_command="$JAVA_HOME/bin/java"; fi
exec "$java_command" -classpath "$project_dir/wrapper/gradle-wrapper.jar" org.gradle.wrapper.GradleWrapperMain -p "$project_dir" assembleDebug "$@"
