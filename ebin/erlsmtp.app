{
    application,
    erlsmtp,
    [
        {description, "A simple SMTP Server in erlang"},
        {vsn, "1.0"},
        {mod, {erlsmtp, []}},
        {registered, [erlsmtp_sup]},
        {modules, [
            erlsmtp,
            erlsmtp_sup,
            erlsmtp_serv
        ]},
        {applications, [stdlib, kernel, crypto, public_key, asn1, ssl]},
        {env, [
            {port, 25},
            {port_ssl, 465},
            {address, "localhost"}
        ]}
    ]
}.