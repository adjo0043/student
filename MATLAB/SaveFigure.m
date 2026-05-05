% ====================================================================================
% SaveFigure : mise en forme et export d'une figure MATLAB.
%
% Inspired by: "Make Great MATLAB Figures for your Scientific Paper or your PhD Thesis"
% https://www.youtube.com/watch?v=wP3jjk1O18A
%
% Syntaxe :
%   SaveFigure(hfig, fname)
%   SaveFigure(hfig, fname, 'Resolution', 300)
%
% Entrees :
%   hfig  : handle de la figure a sauvegarder
%   fname : chemin du fichier de sortie avec extension
%           (.pdf, .eps, .png, .tif/.tiff, .jpg/.jpeg)
%
% Options :
%   'Resolution', dpi      : resolution en DPI (defaut : 600)
%   'WidthCm', width       : largeur de la figure en centimetres
%   'HeightCm', height     : hauteur de la figure en centimetres
%   'HwRatio', ratio       : ratio hauteur / largeur si HeightCm absent
%   'PreserveLineWidth'    : conserve les epaisseurs deja definies
%   'PreserveMarkerSize'   : conserve les tailles de marqueurs deja definies
%   'AdjustMargins'        : ajuste les marges des axes (defaut : true)
%   'Interpreter'          : interpreteur du texte ('latex', 'none', ...)
%
% Auteur : Djousse Tedongmene Alex / Refine par AI Assistant
% Mis a jour : 2025-06-01
% =====================================================================================

function SaveFigure(hfig, fname, varargin)

    % --- Validation des entrees ---
    if ~ishandle(hfig) || ~strcmp(get(hfig, 'Type'), 'figure')
        error('hfig doit etre un handle de figure valide.');
    end
    if ~ischar(fname) && ~isstring(fname) || isempty(fname)
        error('fname doit etre une chaine non vide.');
    end
    fname = char(fname);

    % --- Lecture des options ---
    dpi = 300;
    customWidthCm = [];
    customHeightCm = [];
    customHwRatio = [];
    preserveLineWidth = false;
    preserveMarkerSize = false;
    adjustMargins = true;
    textInterpreter = 'latex';

    for i = 1:2:length(varargin)
        if i + 1 > length(varargin)
            warning('Argument optionnel sans valeur ignore : %s', char(varargin{i}));
            break;
        end

        optName = char(varargin{i});
        optValue = varargin{i+1};

        switch lower(optName)
            case {'resolution', 'dpi'}
                if isnumeric(optValue) && isscalar(optValue) && optValue > 0
                    dpi = optValue;
                else
                    warning('Valeur de resolution invalide. Utilisation du defaut %d dpi.', dpi);
                end
            case {'widthcm', 'width', 'picturewidthcm'}
                if isnumeric(optValue) && isscalar(optValue) && optValue > 0
                    customWidthCm = optValue;
                else
                    warning('Largeur invalide ignoree.');
                end
            case {'heightcm', 'height'}
                if isnumeric(optValue) && isscalar(optValue) && optValue > 0
                    customHeightCm = optValue;
                else
                    warning('Hauteur invalide ignoree.');
                end
            case {'hwratio', 'heightwidthratio'}
                if isnumeric(optValue) && isscalar(optValue) && optValue > 0
                    customHwRatio = optValue;
                else
                    warning('Ratio hauteur/largeur invalide ignore.');
                end
            case 'preservelinewidth'
                preserveLineWidth = logical(optValue);
            case 'preservemarkersize'
                preserveMarkerSize = logical(optValue);
            case 'adjustmargins'
                adjustMargins = logical(optValue);
            case 'interpreter'
                textInterpreter = char(optValue);
            otherwise
                warning('Argument optionnel inconnu : %s', optName);
        end
    end
    resolution_str = sprintf('-r%d', dpi);

    % --- Nettoyage du nom de fichier ---
    [fpath, fname_base, fext] = fileparts(fname);
    fname_base = regexprep(fname_base, '[/\\*:?"<>|]', '_');
    fname_base = strrep(fname_base, '..', '_');
    fname = fullfile(fpath, [fname_base, fext]);

    % --- Parametres de style ---
    picturewidth_cm = 15;
    hw_ratio        = 3/4;
    fontSize        = 10;
    lineWidth       = 1.0;
    markerSize      = 6;

    % --- Detection du type de contenu (heatmap ou axes standard) ---
    hm = findall(hfig, 'Type', 'HeatmapChart');
    ax = findall(hfig, 'Type', 'Axes');
    is_heatmap = ~isempty(hm);

    % --- Application du style aux objets standards ---
    if ~is_heatmap
        set(findall(hfig, '-property', 'FontSize'), 'FontSize', fontSize);
        set(findall(hfig, '-property', 'FontName'), 'FontName', 'Times New Roman');
        if ~preserveLineWidth
            set(findall(hfig, '-property', 'LineWidth'), 'LineWidth', lineWidth);
        end
        if ~preserveMarkerSize
            set(findall(hfig, '-property', 'MarkerSize'), 'MarkerSize', markerSize);
        end

        for i = 1:length(ax)
            ca = ax(i);
            set(ca, 'FontSize', fontSize, 'FontName', 'Times New Roman', ...
                'LineWidth', lineWidth * 0.75, 'Box', 'on', ...
                'TickDir', 'out', 'TickLabelInterpreter', textInterpreter, ...
                'XMinorTick', 'on', 'YMinorTick', 'on');

            if isprop(ca, 'XLabel') && ishandle(ca.XLabel)
                set(ca.XLabel, 'Interpreter', textInterpreter, 'FontSize', fontSize);
            end
            if isprop(ca, 'YLabel') && ishandle(ca.YLabel)
                set(ca.YLabel, 'Interpreter', textInterpreter, 'FontSize', fontSize);
            end
            if isprop(ca, 'Title') && ishandle(ca.Title)
                set(ca.Title, 'Interpreter', textInterpreter, 'FontSize', fontSize * 1.1, 'FontWeight', 'bold');
            end
        end

        % Style de la legende
        leg = findall(hfig, 'Type', 'Legend');
        if ~isempty(leg)
            set(leg, 'Interpreter', textInterpreter, 'FontSize', fontSize * 0.9, 'Box', 'on');
        end
    else
        % Heatmap : seules les proprietes compatibles
        for i = 1:length(hm)
            hm(i).FontSize = fontSize;
        end
    end

    % --- Dimensions de la figure ---
    if ~isempty(customWidthCm)
        picturewidth_cm = customWidthCm;
    end
    if ~isempty(customHwRatio)
        hw_ratio = customHwRatio;
    end

    figWidth = picturewidth_cm;
    if ~isempty(customHeightCm)
        figHeight = customHeightCm;
    else
        figHeight = picturewidth_cm * hw_ratio;
    end

    set(hfig, 'Units', 'centimeters');
    figPos = get(hfig, 'Position');
    set(hfig, 'Position', [figPos(1) figPos(2) figWidth figHeight]);

    set(hfig, 'PaperUnits', 'centimeters');
    set(hfig, 'PaperSize', [figWidth figHeight]);
    set(hfig, 'PaperPositionMode', 'manual');
    set(hfig, 'PaperPosition', [0 0 figWidth figHeight]);

    % --- Reduction des marges (axes standard uniquement) ---
    if adjustMargins && ~is_heatmap && ~isempty(ax)
        try
            for i = 1:length(ax)
                outerpos  = get(ax(i), 'OuterPosition');
                ti        = get(ax(i), 'TightInset');
                left      = outerpos(1) + ti(1);
                bottom    = outerpos(2) + ti(2);
                ax_width  = outerpos(3) - ti(1) - ti(3);
                ax_height = outerpos(4) - ti(2) - ti(4);
                set(ax(i), 'Position', [left bottom ax_width ax_height]);
            end
        catch ME
            warning(ME.identifier, 'Ajustement des marges impossible : %s', ME.message);
        end
    end

    % --- Export ---
    fprintf('Sauvegarde de la figure : %s\n', fname);

    switch lower(fext)
        case '.pdf'
            print(hfig, fname, '-dpdf', '-vector', resolution_str);
        case '.eps'
            print(hfig, fname, '-depsc', '-vector', resolution_str);
        case '.png'
            print(hfig, fname, '-dpng', '-image', resolution_str);
        case {'.tif', '.tiff'}
            print(hfig, fname, '-dtiff', '-image', resolution_str);
        case {'.jpg', '.jpeg'}
            % Note : print() ne supporte pas -q95. Qualite par defaut.
            print(hfig, fname, '-djpeg', '-image', resolution_str);
        otherwise
            warning('Extension non supportee : %s. Figure non sauvegardee.', fext);
            return;
    end

    fprintf('Figure sauvegardee (%s, %d dpi).\n', upper(fext(2:end)), dpi);
end
