package Web::Controller::Role::API;

use My::Moose::Role;
use Future::AsyncAwait;

use header;

async sub get_input ($self, $ctx)
{
	return await $ctx->req->json;
}

async sub respond ($self, $ctx, $status, $data)
{
	my %ret = (
		status => $status,
		data => $data,
	);

	await $ctx->res->json(\%ret);
	return;
}

