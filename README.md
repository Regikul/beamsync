beamsync
=====

A rebar plugin

Build
-----

    $ rebar3 compile

Use
---

Add the plugin to your rebar config:

    {plugins, [
        {beamsync, {git, "https://host/user/beamsync.git", {tag, "0.1.0"}}}
    ]}.

Then just call your plugin directly in an existing application:


    $ rebar3 beamsync
    ===> Fetching beamsync
    ===> Compiling beamsync
    <Plugin Output>
