requires 'Moose' => 0;
requires 'Mouse' => 0;
requires 'Util::H2O' => 0;
requires 'Mooish::AttributeBuilder' => 0;
requires 'Sub::HandlesVia' => 0;
requires 'Hook::AfterRuntime' => 0;

requires 'Beam::Wire' => 0;
requires 'Dotenv' => 0;
requires 'Data::Localize' => 0;

requires 'Form::Tiny' => '2.06';
requires 'Form::Tiny::Plugin::Diva' => 0;

requires 'Type::Tiny' => 0;
requires 'Type::Libraries' => 0;
requires 'Types::DateTime' => 0;
requires 'Type::EmailAddress' => 0;

requires 'Mojolicious' => 0;

requires 'DBI' => 0;
requires 'DBD::Pg' => 0;
requires 'Mojo::Redis' => 0;
requires 'DBIx::Class' => 0;
requires 'DateTime::Format::Pg' => 0;

requires 'Import::Into' => 0;
requires 'namespace::autoclean' => 0;
requires 'true' => 0;

requires 'Log::Dispatch' => 0;
requires 'MojoX::Log::Dispatch::Simple' => 0;

requires 'Ref::Util' => 0;
requires 'List::Util' => 0;

requires 'Sereal::Encoder' => 0;
requires 'Sereal::Decoder' => 0;

requires 'Data::Entropy' => 0;
requires 'Data::ULID' => 0;
requires 'Quantum::Superpositions::Lazy' => 0;
requires 'Crypt::Bcrypt' => 0;

requires 'Text::PO' => 0;
requires 'Text::Levenshtein::BV' => 0;

on 'test' => sub {
	requires 'Test::DB';
	requires 'Test2::Harness';
	requires 'Test2::V0';
	requires 'Test::Spy';
};

# vim: ft=perl

