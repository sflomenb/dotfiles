#!/usr/bin/env python3

import subprocess
import iterm2


async def main(connection):
    PROFILE_NAME = subprocess.check_output(
        [
            "osascript",
            "/Users/sflomenb/Library/Mobile Documents/com~apple~ScriptEditor2/Documents/Light-Dark Terminal.scpt",
        ],
        stderr=subprocess.STDOUT,
    )

    if not PROFILE_NAME:
        return
    PROFILE_NAME = bytes.decode(PROFILE_NAME).strip()
    if PROFILE_NAME not in ["Gruvbox Light", "Gruvbox Dark"]:
        return

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
