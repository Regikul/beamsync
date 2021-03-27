beamsync
=====

A rebar plugin to do remote load for changed beam modules.

Configure
-----
Add this to your rebar config:

    {beamsync, [
        {mode, longname | shortname},
        {setcookie, 'your_super_secret_cookie'},
        {nodes, ['your_node@somewhere']}
    ]}.

Use
---

Add the plugin to your rebar config:

    {plugins, [
        {beamsync, {git, "https://github.com/regikul/beamsync.git", {tag, "0.1.0"}}}
    ]}.

Then just call your plugin directly in an existing application:


    $ rebar3 beamsync
    ===> Fetching beamsync
    ===> Compiling beamsync
    <Plugin Output>
