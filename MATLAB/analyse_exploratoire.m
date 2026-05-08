% =======================================================================
% ANALYSE EXPLORATOIRE - Performance des etudiants (Math)
% Dataset : student-mat.csv (UCI / P. Cortez)
% Auteur : Djousse Tedongmene Alex
% =======================================================================
clear; close all; clc;

%% 1. CHARGEMENT ET PREPARATION
% =======================================================================
% Description : 
%   Charge les données brutes et effectue le typage des colonnes.
%
% Motivation :
%   Les modèles de régression et les fonctions de visualisation requièrent 
%   une distinction stricte entre variables quantitatives (double) et 
%   qualitatives (categorical) pour garantir la justesse des calculs.
%
% Sortie : 
%   Table `df` typée, listes `num_cols` et `cat_cols`.
% =======================================================================

% Detection du fichier
if exist('studentmat.csv', 'file')
    data_path = 'studentmat.csv';
elseif exist('student-mat.csv', 'file')
    data_path = 'student-mat.csv';
else
    error('Fichier studentmat.csv ou student-mat.csv introuvable.');
end

opts = detectImportOptions(data_path, 'Delimiter', ';');
df = readtable(data_path, opts);

% Conversion des colonnes texte en categorielles
varNames = df.Properties.VariableNames;
for i = 1:length(varNames)
    if iscell(df.(varNames{i})) || isstring(df.(varNames{i}))
        df.(varNames{i}) = categorical(df.(varNames{i}));
    end
end

target = 'G3';
[n_obs, n_vars] = size(df);
fprintf('Dataset charge : %d observations, %d variables.\n\n', n_obs, n_vars);

% Classification des colonnes
num_cols = {};
cat_cols = {};
for i = 1:length(varNames)
    if isnumeric(df.(varNames{i}))
        num_cols{end+1} = varNames{i}; %#ok<SAGROW>
    else
        cat_cols{end+1} = varNames{i}; %#ok<SAGROW>
    end
end

% Themes thematiques
themes = struct();
themes.Academic     = {'school', 'reason', 'schoolsup', 'famsup', 'paid', ...
                       'activities', 'nursery', 'higher', 'traveltime', ...
                       'studytime', 'failures', 'absences', 'G1', 'G2'};
themes.Demographics = {'sex', 'address', 'famsize', 'Pstatus', 'Mjob', ...
                       'Fjob', 'guardian', 'age', 'Medu', 'Fedu', 'famrel'};
themes.Lifestyle    = {'internet', 'romantic', 'freetime', 'goout', ...
                       'Dalc', 'Walc', 'health'};

% Repertoire de sortie
output_dir = 'AnalyseExploratoire';
if ~exist(output_dir, 'dir'), mkdir(output_dir); end
fprintf('Sorties dans le dossier : %s/\n\n', output_dir);

% Couleur principale
matlab_blue = [0 0.45 0.74];
red_color   = [0.84 0.16 0.16];
green_color = [0.17 0.63 0.17];


%% 2. RESUME GLOBAL DU DATASET
% =====================================================================
% Description : 
%   Génère un tableau de synthèse recensant les types, le nombre de 
%   valeurs uniques et les valeurs manquantes pour chaque variable.
%
% Motivation :
%   L'absence de valeurs manquantes doit être formellement vérifiée 
%   avant de valider qu'aucune stratégie d'imputation n'est requise.
%   L'identification des élèves à G3 = 0 isole la masse anormale cible.
%
% Sortie : 
%   Fichiers '00_Global_Summary.csv' et '01_G3_Zero_Cases.csv'.
% =====================================================================
fprintf('--- Resume global du dataset ---\n');
GlobalSummary = table('Size', [length(varNames) 4], ...
    'VariableTypes', {'cell', 'cell', 'double', 'double'}, ...
    'VariableNames', {'Variable', 'Type', 'N_Unique', 'N_Missing'});

for i = 1:length(varNames)
    vn = varNames{i};
    if isnumeric(df.(vn))
        vtype = 'numeric';
        n_unique = length(unique(df.(vn)(~isnan(df.(vn)))));
        n_miss   = sum(isnan(df.(vn)));
    else
        vtype = 'categorical';
        n_unique = length(categories(df.(vn)));
        n_miss   = sum(isundefined(df.(vn)));
    end
    GlobalSummary.Variable{i} = vn;
    GlobalSummary.Type{i} = vtype;
    GlobalSummary.N_Unique(i) = n_unique;
    GlobalSummary.N_Missing(i) = n_miss;
end
writetable(GlobalSummary, fullfile(output_dir, '00_Global_Summary.csv'));
fprintf('  -> 00_Global_Summary.csv exporte.\n');

% Signaler les G3 = 0
n_zero = sum(df.(target) == 0);
fprintf('  -> Observations avec G3 = 0 : %d / %d (%.1f%%)\n', ...
    n_zero, n_obs, 100 * n_zero / n_obs);

zero_vars = {'G1', 'G2', 'failures', 'absences', 'school', 'sex', 'higher'};
zero_vars = intersect(zero_vars, varNames, 'stable');

G3ZeroCases = df(df.G3 == 0, zero_vars);
writetable(G3ZeroCases, fullfile(output_dir, '01_G3_Zero_Cases.csv'));
fprintf('  -> 01_G3_Zero_Cases.csv exporte.\n\n');


%% 3. DISTRIBUTION DE LA CIBLE (G3)
% ========================================================================
% Description : 
%   Trace l'histogramme de la note finale et calcule ses moments spatiaux.
%
% Motivation :
%   La visualisation de l'asymétrie de la distribution de G3 est le 
%   prérequis mathématique pour justifier que les résidus d'une 
%   régression linéaire (OLS) ne suivront pas une loi normale.
%
% Sortie : 
%   Figure '01_G3_Distribution.png' et '01_G3_Stats.csv'.
% =======================================================================
fprintf('--- Distribution de G3 ---\n');

g3 = df.(target);
mu_g3 = mean(g3, 'omitnan');
md_g3 = median(g3, 'omitnan');

fig = figure('Visible', 'off');
hHist = histogram(g3, 0:1:21, 'FaceColor', matlab_blue, 'EdgeColor', 'w', ...
                  'HandleVisibility', 'off');
hold on;

% Créer les xline et stocker leurs handles
hMean = xline(mu_g3, '--', 'LineWidth', 1.5, 'Color', red_color);
hMed  = xline(md_g3, '-.', 'LineWidth', 1.5, 'Color', green_color);

% Assigner des noms affichés (DisplayName) sur les lignes
hMean.DisplayName = sprintf('Moyenne = %.2f', mu_g3);
hMed.DisplayName  = sprintf('Mediane = %.1f', md_g3);

xlabel('Note finale G3');
ylabel('Effectif');
title('Distribution de G3');

% Appeler legend en passant explicitement les handles dans l'ordre voulu
legend([hMean, hMed], 'Location', 'northeast');

xlim([-0.5 21]);
SaveFigure(fig, fullfile(output_dir, '01_G3_Distribution.png'));
close(fig);

% Stats descriptives de G3
target_stats = table(n_obs, mu_g3, md_g3, std(g3, 'omitnan'), ...
    min(g3), max(g3), n_zero, ...
    'VariableNames', {'N', 'Mean', 'Median', 'Std', 'Min', 'Max', 'N_Zero'});
writetable(target_stats, fullfile(output_dir, '01_G3_Stats.csv'));
fprintf('  -> Figures et CSV de G3 exportes.\n');


%% 4. RESUMES STATISTIQUES PAR THEME
% =====================================================================
% Description : 
%   Calcule les mesures de tendance centrale, dispersion et quantiles 
%   par regroupement thématique (Académique, Démographique, Mode de vie).
%
% Motivation :
%   Segmenter l'exploration par thème permet d'isoler les caractéristiques 
%   structurelles asymétriques (ex: `failures`, `absences`) avant leur 
%   incorporation dans un modèle prédictif commun.
% =====================================================================
fprintf('\n--- Resumes par theme ---\n');
themeNames = fieldnames(themes);
for t = 1:length(themeNames)
    tName     = themeNames{t};
    tFeatures = themes.(tName);

    % A. Variables numeriques
    tNum = intersect(tFeatures, num_cols, 'stable');
    if ~isempty(tNum)
        SumTbl = table();
        for j = 1:length(tNum)
            col  = tNum{j};
            data = df.(col);
            stats_row = [sum(~isnan(data)), mean(data, 'omitnan'), ...
                         std(data, 'omitnan'), min(data), ...
                         prctile(data, 25), median(data, 'omitnan'), ...
                         prctile(data, 75), max(data)];
            row = array2table(stats_row, ...
                'VariableNames', {'N', 'Mean', 'Std', 'Min', 'Q1', ...
                                  'Median', 'Q3', 'Max'});
            row.Feature = {col};
            row = row(:, [{'Feature'}, ...
                          row.Properties.VariableNames(1:end-1)]);
            SumTbl = [SumTbl; row]; %#ok<AGROW>
        end
        writetable(SumTbl, fullfile(output_dir, ...
            sprintf('02_%s_Num_Summary.csv', tName)));
        fprintf('  -> %s : resume numerique.\n', tName);
    end

    % B. Variables categorielles
    tCat = intersect(tFeatures, cat_cols, 'stable');
    if ~isempty(tCat)
        CatTbl = table();
        for j = 1:length(tCat)
            col = tCat{j};
            cats_col  = categories(df.(col));
            counts = countcats(df.(col));
            pcts   = 100 * counts / sum(counts);
            tmp    = table(repmat({col}, length(cats_col), 1), ...
                cats_col, counts, pcts, ...
                'VariableNames', {'Feature', 'Category', 'Count', 'Pct'});
            CatTbl = [CatTbl; tmp]; %#ok<AGROW>
        end
        writetable(CatTbl, fullfile(output_dir, ...
            sprintf('02_%s_Cat_Summary.csv', tName)));
        fprintf('  -> %s : resume categoriel.\n', tName);
    end
end

%% 5. MATRICE DE CORRELATION
% =====================================================================
% Description : 
%   Génère et trace la matrice des corrélations linéaires de Pearson 
%   entre toutes les variables quantitatives.
%
% Motivation :
%   Détecter la multicolinéarité sévère (notamment entre G1 et G2) est 
%   impératif pour anticiper l'instabilité de la variance des 
%   estimateurs (inflation) lors des régressions multiples.
% =====================================================================
fprintf('\n--- Matrice de correlation ---\n');
num_data = df(:, num_cols);
corr_mat = corr(table2array(num_data), 'Rows', 'pairwise');
% Figure heatmap
fig = figure('Visible', 'off', 'Position', [100 100 1100 900]);
imagesc(corr_mat);
colorbar;
n_levels = 256;
half = n_levels / 2;
red_blue = [
    [linspace(0.13, 1, half)', linspace(0.40, 1, half)', linspace(0.67, 1, half)'];
    [linspace(1, 0.70, half)', linspace(1, 0.09, half)', linspace(1, 0.16, half)']
];
colormap(red_blue);
clim([-1 1]);
set(gca, 'XTick', 1:length(num_cols), 'XTickLabel', num_cols, ...
         'YTick', 1:length(num_cols), 'YTickLabel', num_cols, ...
         'XTickLabelRotation', 45, 'TickLabelInterpreter', 'none');
title('Matrice de correlation (variables numeriques)');

% Annotations des valeurs dans les cellules
for i = 1:size(corr_mat, 1)
    for j = 1:size(corr_mat, 2)
        if abs(corr_mat(i, j)) > 0.5
            clr = 'w';
        else
            clr = 'k';
        end
        text(j, i, sprintf('%.2f', corr_mat(i, j)), ...
            'HorizontalAlignment', 'center', 'FontSize', 6, 'Color', clr);
    end
end
SaveFigure(fig, fullfile(output_dir, '03_Correlation_Matrix.png'), ...
    'Resolution', 300);
close(fig);

% Export CSV
corr_tbl = array2table(corr_mat, 'VariableNames', num_cols, ...
    'RowNames', num_cols);
writetable(corr_tbl, fullfile(output_dir, '03_Correlation_Matrix.csv'), ...
    'WriteRowNames', true);
fprintf('  -> Matrice de correlation exportee.\n');


%% 6. ANALYSE BIVARIEE : predicteurs vs G3
% =====================================================================
% Description : 
%   Visualise les associations marginales entre chaque prédicteur et 
%   la variable cible via des boîtes à moustaches et des nuages de points.
%
% Motivation :
%   Évaluer le pouvoir explicatif brut individuel avant que les effets ne 
%   soient conditionnés par d'autres variables dans un modèle complet.
% =====================================================================
fprintf('\n--- Analyse bivariee ---\n');
% Variables categorielles vs G3 (boxplots)
biv_cat = table();
for j = 1:length(cat_cols)
    feat = cat_cols{j};

    fig = figure('Visible', 'off');
    boxchart(df.(feat), df.(target), 'BoxFaceColor', matlab_blue, ...
        'BoxWidth', 0.55, 'MarkerSize', 3);
    xlabel(strrep(feat, '_', ' '));
    ylabel('G3');
    title(sprintf('G3 par %s', feat));
    cats_feat = categories(df.(feat));
    if max(cellfun(@length, cats_feat)) > 6 || length(cats_feat) > 4
        xtickangle(30);
    end
    SaveFigure(fig, fullfile(output_dir, sprintf('04_Box_%s.png', feat)));
    close(fig);

    % Stats par modalite
    [G, cats_seg] = findgroups(df.(feat));
    mn = splitapply(@(x) mean(x, 'omitnan'), df.(target), G);
    md = splitapply(@(x) median(x, 'omitnan'), df.(target), G);
    sd = splitapply(@(x) std(x, 'omitnan'), df.(target), G);
    ct = splitapply(@(x) sum(~isnan(x)), df.(target), G);
    tmp = table(repmat({feat}, length(cats_seg), 1), cats_seg, ct, mn, md, sd, ...
        'VariableNames', {'Feature', 'Category', 'N', 'Mean_G3', ...
                          'Median_G3', 'Std_G3'});
    biv_cat = [biv_cat; tmp]; %#ok<AGROW>
end
writetable(biv_cat, fullfile(output_dir, '04_Bivariate_Cat_vs_G3.csv'));
fprintf('  -> Boxplots et CSV categoriels exportes.\n');

% 6 Variables numeriques vs G3 (scatter + regression simple)
biv_num = table();
rng_jitter = RandStream('mt19937ar', 'Seed', 42);

for j = 1:length(num_cols)
    feat = num_cols{j};
    if strcmp(feat, target), continue; end

    fig = figure('Visible', 'off');
    valid = ~isnan(df.(feat)) & ~isnan(df.(target));
    x = df.(feat)(valid);
    y = df.(target)(valid);

    % Jitter horizontal pour predicteurs entiers a faible cardinalite
    if all(x == round(x)) && length(unique(x)) <= 25
        jitter = 0.08 * randn(rng_jitter, length(x), 1);
    else
        jitter = zeros(length(x), 1);
    end

    scatter(x + jitter, y, 18, matlab_blue, 'filled', ...
        'MarkerFaceAlpha', 0.45);
    hold on;

    p = polyfit(x, y, 1);
    xfit = linspace(min(x), max(x), 100);
    plot(xfit, polyval(p, xfit), '-', 'LineWidth', 1.5, 'Color', red_color);

    [r, pval] = corr(x, y);
    legend(sprintf('OLS: y = %.2f + %.3f x  (r = %.2f)', p(2), p(1), r), ...
        'Location', 'best');

    xlabel(strrep(feat, '_', ' '));
    ylabel('G3');
    title(sprintf('G3 vs %s', feat));
    SaveFigure(fig, fullfile(output_dir, sprintf('05_Scatter_%s.png', feat)));
    close(fig);

    tmp = table({feat}, r, pval, p(1), ...
        'VariableNames', {'Feature', 'Corr_G3', 'P_Value', 'Slope'});
    biv_num = [biv_num; tmp]; %#ok<AGROW>
end
writetable(biv_num, fullfile(output_dir, '05_Bivariate_Num_vs_G3.csv'));
fprintf('  -> Scatterplots et CSV numeriques exportes.\n');


%% 7. COMPARAISONS DE SOUS-ECHANTILLONS
% =====================================================================
% Description : 
%   Croise la variable de performance finale avec les attributs 
%   catégoriels majeurs (sexe, localisation, souhait d'études supérieures).
%
% Motivation :
%   Mettre en évidence les biais de représentativité dans la cohorte
%   qui affecteront directement l'erreur standard des estimateurs par 
%   sous-groupe lors des régressions.
% =====================================================================
fprintf('\n--- Comparaisons de sous-echantillons ---\n');
segments = {'sex', 'school', 'address', 'higher', ...
            'romantic', 'internet', 'Pstatus'};
for s = 1:length(segments)
    seg = segments{s};
    cats_seg = categories(df.(seg));
    n_cat = length(cats_seg);

    % Histogrammes superposes de G3 par sous-echantillon
    fig = figure('Visible', 'off');
    hold on;
    colors = lines(n_cat);
    for k = 1:n_cat
        mask = df.(seg) == cats_seg{k};
        histogram(df.(target)(mask), 0:1:21, ...
            'FaceColor', colors(k, :), 'FaceAlpha', 0.55, ...
            'EdgeColor', 'w', 'LineWidth', 0.6);
    end
    xlabel('G3');
    ylabel('Effectif');
    title(sprintf('Distribution de G3 par %s', seg));
    legend(cats_seg, 'Location', 'northeast');
    xlim([-0.5 21]);
    hold off;
    SaveFigure(fig, fullfile(output_dir, sprintf('06_Hist_G3_by_%s.png', seg)));
    close(fig);

    % Boxplots cote a cote pour les variables numeriques cles
    key_vars = {'G1', 'G2', 'studytime', 'failures', 'absences', 'age'};
    key_vars = intersect(key_vars, num_cols, 'stable');
    n_kv = length(key_vars);

    fig = figure('Visible', 'off', 'Position', [100 100 1800 450], ...
        'Color', 'w');
    tl = tiledlayout(1, n_kv, ...
        'TileSpacing', 'compact', ...
        'Padding', 'compact');

    for k = 1:n_kv
        ax_k = nexttile(tl);
        boxchart(ax_k, df.(seg), df.(key_vars{k}), ...
            'BoxFaceColor', matlab_blue, ...
            'BoxWidth', 0.5, 'MarkerSize', 2);
        ylabel(ax_k, key_vars{k}, 'FontSize', 10);
        title(ax_k, key_vars{k}, 'FontSize', 11, 'FontWeight', 'bold');
        if k == 1
            xlabel(ax_k, seg, 'FontSize', 10);
        else
            xlabel(ax_k, '');
        end
        set(ax_k, 'FontSize', 9, 'FontName', 'Times New Roman', ...
            'TickDir', 'out', 'Box', 'on');
        xtickangle(ax_k, 0);
    end
    title(tl, sprintf('Variables numeriques cles par %s', seg), ...
        'FontSize', 12, 'FontWeight', 'bold', ...
        'FontName', 'Times New Roman');

    % Sauvegarde directe (bypass SaveFigure.m pour preserver le tiledlayout)
    out_file = fullfile(output_dir, sprintf('07_Compare_%s.png', seg));
    exportgraphics(fig, out_file, 'Resolution', 250, ...
        'BackgroundColor', 'white');
    fprintf('Figure sauvegardee : %s (PNG, 250 dpi).\n', out_file);
    close(fig);

    fprintf('  -> Sous-echantillons par %s : histogrammes + boxplots.\n', seg);
end

% Tableau comparatif : moyenne de G3 par segmentation
CompTable = table();
for s = 1:length(segments)
    seg = segments{s};
    [G, cats_seg] = findgroups(df.(seg));
    mn = splitapply(@(x) mean(x, 'omitnan'), df.(target), G);
    sd = splitapply(@(x) std(x, 'omitnan'), df.(target), G);
    ct = splitapply(@(x) numel(x), df.(target), G);
    tmp = table(repmat({seg}, length(cats_seg), 1), cats_seg, ct, mn, sd, ...
        'VariableNames', {'Segment', 'Category', 'N', 'Mean_G3', 'Std_G3'});
    CompTable = [CompTable; tmp]; %#ok<AGROW>
end
writetable(CompTable, fullfile(output_dir, '07_Subsample_Comparison.csv'));
fprintf(' Tableau comparatif exporte.\n');



%% 8. SCATTER MATRIX DES PREDICTEURS CLES
% =====================================================================
% Description : 
%   Produit une matrice de nuages de points pour les 5 variables les 
%   plus corrélées avec G3, ainsi que G3 elle-même.
%
% Motivation :
%   L'inspection visuelle conjointe révèle des structures non-linéaires, 
%   la discrétisation stricte de certaines échelles (ex: failures), et
%   aide à décider s'il faut inclure des termes polynomiaux d'interaction.
% =====================================================================
fprintf('\n--- Scatter matrix ---\n');
% Variables les plus correlees a G3 (en valeur absolue, hors G3)
g3_idx = find(strcmp(num_cols, 'G3'));
abs_corr_g3 = abs(corr_mat(:, g3_idx));
abs_corr_g3(g3_idx) = -Inf;  % exclure G3 elle-meme
[~, idx_sort] = sort(abs_corr_g3, 'descend');

% Prendre les 5 premieres puis ajouter G3 a la fin
top_vars = num_cols(idx_sort(1:5));
top_vars{end+1} = 'G3';

fig = figure('Visible', 'off', 'Position', [100 100 1100 1000], ...
    'Color', 'w');
top_data = table2array(df(:, top_vars));
[~, ax_mat] = plotmatrix(top_data);
for i = 1:length(top_vars)
    ylabel(ax_mat(i, 1), top_vars{i}, 'FontSize', 8, 'Interpreter', 'none');
    xlabel(ax_mat(end, i), top_vars{i}, 'FontSize', 8, 'Interpreter', 'none');
end
sgtitle('Scatter matrix : predicteurs les plus correles a G3', ...
    'FontSize', 11, 'FontWeight', 'bold');

% Sauvegarde directe
out_file = fullfile(output_dir, '08_Scatter_Matrix_Top.png');
exportgraphics(fig, out_file, 'Resolution', 250, ...
    'BackgroundColor', 'white');
fprintf('Figure sauvegardee : %s (PNG, 250 dpi).\n', out_file);
close(fig);
fprintf('  -> Scatter matrix exportee.\n');


% FIN
fprintf('\n=========================================\n');
fprintf('Analyse exploratoire terminee.\n');
fprintf('Tous les fichiers sont dans : %s/\n', output_dir);
fprintf('=========================================\n');
