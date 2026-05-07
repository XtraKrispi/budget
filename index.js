import { Elm } from "./src/Main.elm";
import { createClient } from '@supabase/supabase-js';

const app = Elm.Main.init();

const supabase = createClient('https://cpmycpfuezkmecrryepz.supabase.co', 'sb_publishable_-lju4m81D5thK2S-E7ITDQ_3lNJXLgX')

app.ports.openDialog.subscribe((id) => {
    document.getElementById(id).showModal();
});

app.ports.signUp.subscribe(async ({ email, password }) => {
    const results = await supabase.auth.signUp({ email: email, password: password });
    if (results.error) {
        app.ports.onSignUpFailure.send(results.error.message);
    } else
        app.ports.onSignUpSuccess.send({ email: email, userId: results.id, confirmed: false });
});

app.ports.signIn.subscribe(async ({ email, password }) => {
    const results = await supabase.auth.signInWithPassword({ email: email, password: password });
    if (results.error) {
        app.ports.onLoginFailure.send(results.error.message);
    } else {
        app.ports.onLoginSuccess.send({ email: email, userId: results.data.user.id, confirmed: !!results.data.user.email_confirmed_at });
    }
});

app.ports.initiateGetUser.subscribe(async () => {
    const results = await supabase.auth.getUser();
    if (results.error) {
        app.ports.gotUser.send(null);
    } else {
        app.ports.gotUser.send({ email: results.data.user.email, userId: results.data.user.id, confirmed: !!results.data.user.email_confirmed_at });
    }

});

app.ports.copyToClipboard.subscribe((text) => {
    if ('clipboard' in window.navigator) {
        navigator.clipboard.writeText(text);
    }
});

app.ports.fetchDefinitions.subscribe(async () => {
    const { data, error } = await supabase.from("definitions").select();
    if (error) {
        app.ports.fetchDefinitionsFailure.send(error.message);
    } else {
        app.ports.fetchDefinitionsSuccess.send(data.map((def) => ({
            startDate: def.start_date
            , endDate: def.end_date
            , description: def.description
            , amount: def.amount
            , frequency: def.frequency
            , isAutomatic: def.is_automatic_withdrawal
            , id: def.id
        })));
    }
});

app.ports.fetchScratch.subscribe(async () => {
    const { data, error } = await supabase.from("scratch").select().limit(1).maybeSingle();
    if (error) {
        app.ports.fetchScratchFailure.send(error.message);
    } else {
        if (data) {
            app.ports.fetchScratchSuccess.send({
                endDate: data.end_date,
                amountInBank: data.amount_in_bank,
                amountLeftOver: data.amount_left_over,
                id: data.id
            });

        } else {
            app.ports.fetchScratchSuccess.send(null);
        }
    }
});

app.ports.logout.subscribe(async () => {
    await supabase.auth.signOut();
    app.ports.loggedOut.send({});
});

app.ports.insertScratch.subscribe(async ({ data, userId }) => {
    const results = await supabase.from("scratch").insert({ end_date: data.endDate, amount_in_bank: data.amountInBank, amount_left_over: data.amountLeftOver, user_id: userId }).select().limit(1).single();
    if (results.error) {
        app.ports.saveScratchFailure.send(error.message);
    } else {
        app.ports.saveScratchSuccess.send(results.data.id);
    }
});

app.ports.updateScratch.subscribe(async ({ data, id }) => {
    const results = await supabase.from("scratch").update({ end_date: data.endDate, amount_in_bank: data.amountInBank, amount_left_over: data.amountLeftOver }).eq('id', id);
    if (results.error) {
        app.ports.saveScratchFailure.send(error.message);
    } else {
        app.ports.saveScratchSuccess.send(id);
    }
});

app.ports.fetchArchive.subscribe(async () => {
    const results = await supabase.from("archive").select();
    if (results.error) {
        app.ports.fetchArchiveFailure.send(results.error.message);
    } else {
        app.ports.fetchArchiveSuccess.send(results.data.map((archive) => ({
            date: archive.date,
            definitionId: archive.definition_id,
            description: archive.description,
            amount: archive.amount,
            action: archive.action
        })));
    }
});

app.ports.insertArchive.subscribe(async ({ data, userId }) => {
    const results = await supabase.from("archive").insert({ user_id: userId, definition_id: data.definitionId, description: data.description, amount: data.amount, date: data.date, action: data.action });
    if (results.error) {
        app.ports.insertArchiveFailure.send(error.message);
    } else {
        app.ports.insertArchiveSuccess.send({});
    }
});
