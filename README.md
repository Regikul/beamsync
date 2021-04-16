beamsync
=====

A rebar plugin to do remote load for changed beam modules.
It *does not* update beam files on filesystem, so node reboot will
reset all changes.

Configure
-----
Add this to your rebar config:

    {beamsync, [
        {mode, longname | shortname},
        {setcookie, 'your_super_secret_cookie'},
        {excluded, [
            {modules, [
                this_module,
                that_module
            ]},
            {apps, [
                some_app,
                another_app
            ]}
        ]},
        {nodes, ['your_node@somewhere']}
    ]}.

Use
---

Add the plugin to your rebar config:

    {plugins, [
        {beamsync, {git, "https://github.com/regikul/beamsync.git", {tag, "0.2.1"}}}
    ]}.

Then just call your plugin directly in an existing application:


    $ rebar3 beamsync
    ===> Fetching beamsync
    ===> Compiling beamsync
    <Plugin Output>
