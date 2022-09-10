#!/usr/bin/env python3

import subprocess
import iterm2


async def main(connection):
    LIGHT_OR_DARK = subprocess.check_output(
        [
            "osascript",
            "/Users/sflomenb/Library/Mobile Documents/com~apple~ScriptEditor2/Documents/Light-Dark Terminal.scpt",
        ],
        stderr=subprocess.STDOUT,
    )

    if not LIGHT_OR_DARK:
        return
    LIGHT_OR_DARK = bytes.decode(LIGHT_OR_DARK).strip()

    PROFILE_NAME = f"Catppuccin {LIGHT_OR_DARK}"

    print(f"found profile name {PROFILE_NAME}")
    app = await iterm2.async_get_app(connection)

    all_profiles = await iterm2.PartialProfile.async_query(connection)
    for profile in all_profiles:
        if profile.name == PROFILE_NAME:
            # await profile.async_make_default()
            full = await profile.async_get_full_profile()
            await app.current_terminal_window.current_tab.current_session.async_set_profile(
                full
            )
            return


iterm2.run_until_complete(main)
